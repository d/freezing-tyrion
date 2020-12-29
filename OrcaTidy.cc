#include "OrcaTidy.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/YAMLTraits.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

using FileToReplacements = std::map<std::string, clang::tooling::Replacements>;

static const char* const kOwnerAnnotation = "gpos::owner";
static const char* const kPointerAnnotation = "gpos::pointer";

static auto AnnotationType(const char* name) {
  return qualType(hasDeclaration(typeAliasTemplateDecl(hasName(name))));
}

__attribute__((const)) static auto OwnerType() {
  return AnnotationType("::gpos::owner");
}

__attribute__((const)) static auto PointerType() {
  return AnnotationType("::gpos::pointer");
}

__attribute__((const)) static auto RefCountPointerType() {
  auto ref_count_record_decl = cxxRecordDecl(isSameOrDerivedFrom(cxxRecordDecl(
      hasMethod(cxxMethodDecl(hasName("Release"), parameterCountIs(0))))));

  auto ref_count_pointer_type =
      pointsTo(hasCanonicalType(hasDeclaration(ref_count_record_decl)));
  return ref_count_pointer_type;
}

struct Annotator {
  ActionOptions action_options;
  FileToReplacements& replacements;
  clang::ASTContext& ast_context;
  const clang::SourceManager& source_manager;
  const clang::LangOptions& lang_opts;

  void Annotate() {
    if (action_options.Base) AnnotateBaseCases();

    if (action_options.Propagate) Propagate();
  }

 private:
  void Propagate() const {
    for (const auto& bound_nodes : match(
             functionDecl(
                 returns(qualType(unless(OwnerType()), RefCountPointerType())),
                 hasDescendant(returnStmt(hasReturnValue(ignoringParenImpCasts(
                     anyOf(declRefExpr(to(varDecl(hasType(OwnerType())))),
                           callExpr(
                               callee(functionDecl(returns(OwnerType()))))))))))
                 .bind("f"),
             ast_context)) {
      const auto* f = bound_nodes.getNodeAs<clang::FunctionDecl>("f");

      AnnotateFunctionReturnOwner(f);
    }

    PropagateVirtualFunctionReturnType(OwnerType(), kOwnerAnnotation);
    PropagateVirtualFunctionReturnType(PointerType(), kPointerAnnotation);

    for (const auto& bound_nodes :
         match(cxxMethodDecl(
                   isOverride(),
                   forEachOverridden(
                       cxxMethodDecl(hasAnyParameter(hasType(OwnerType())))
                           .bind("base")))
                   .bind("derived"),
               ast_context)) {
      const auto* derived_method =
          bound_nodes.getNodeAs<clang::CXXMethodDecl>("derived");
      const auto* base_method =
          bound_nodes.getNodeAs<clang::CXXMethodDecl>("base");
      for (const auto* base_param : base_method->parameters()) {
        if (match(OwnerType(), base_param->getType(), ast_context).empty())
          continue;
        auto parameter_index = base_param->getFunctionScopeIndex();

        AnnotateParameterOwner(derived_method, parameter_index);
      }
    }

    // Practical intuition: the lifetime of the field pointee is taken care of
    // by the object (presumably in its destructor, or less commonly, in a clean
    // up method). The newly assigned pointee is also owned by the "this"
    // object, hence the caller receives no ownership.
    //
    // Theoretically, this is not foolproof, as adversarial code can
    // perform excessive AddRef after assignment. A manual inspection of all
    // occurrences of the following pattern in ORCA turns up no such usage.

    for (const auto& bound_nodes :
         match(returnStmt(
                   hasReturnValue(ignoringParenImpCasts(FieldReferenceFor(
                       fieldDecl(hasType(OwnerType())).bind("field")))),
                   forFunction(cxxMethodDecl(hasDescendant(binaryOperator(
                                                 hasLHS(FieldReferenceFor(
                                                     equalsBoundNode("field"))),
                                                 hasOperatorName("="))))
                                   .bind("method"))),
               ast_context)) {
      const auto* method =
          bound_nodes.getNodeAs<clang::CXXMethodDecl>("method");
      AnnotateFunctionReturnPointer(method);
    }
  }

  static auto FieldReferenceFor(decltype(fieldDecl().bind("")) field_matcher)
      -> decltype(stmt()) {
    return memberExpr(member(field_matcher),
                      hasObjectExpression(cxxThisExpr()));
  }

  void AnnotateBaseCases() const {
    auto releaseCallExpr = [](auto reference_to_field) {
      auto release = cxxMemberCallExpr(
          callee(cxxMethodDecl(hasName("Release"), parameterCountIs(0))),
          on(reference_to_field));
      auto safe_release = callExpr(
          callee(functionDecl(hasName("SafeRelease"), parameterCountIs(1))),
          hasArgument(0, reference_to_field));
      return callExpr(anyOf(release, safe_release));
    };

    auto owner_field = FieldReferenceFor(
        fieldDecl(unless(hasType(OwnerType()))).bind("owner_field"));
    for (const auto& bound_nodes :
         match(releaseCallExpr(owner_field), ast_context)) {
      if (const auto* field_decl =
              bound_nodes.getNodeAs<clang::FieldDecl>("owner_field")) {
        AnnotateFieldOwner(field_decl);
      }
    }

    for (const auto& bound_nodes : match(
             fieldDecl(
                 unless(hasType(PointerType())), hasType(RefCountPointerType()),
                 hasDeclContext(
                     cxxRecordDecl(
                         optionally(hasMethod(cxxDestructorDecl().bind("d"))))
                         .bind("record")))
                 .bind("field"),
             ast_context)) {
      const auto* field_decl = bound_nodes.getNodeAs<clang::FieldDecl>("field");
      const auto* record =
          bound_nodes.getNodeAs<clang::CXXRecordDecl>("record");
      const auto* dtor = bound_nodes.getNodeAs<clang::CXXDestructorDecl>("d");
      // destructor not visible in this translation unit, leave unannotated
      if (dtor && !dtor->isDefined() && !dtor->isDefaulted()) continue;

      if (dtor) {
        auto reference_to_field = FieldReferenceFor(equalsNode(field_decl));
        if (!match(decl(hasDescendant(releaseCallExpr(reference_to_field))),
                   *dtor, ast_context)
                 .empty())
          continue;

        // Here's the tricky part, we don't release this field in the
        // destructor, but there might still be a release in another method. Not
        // every method is necessarily visible in this translation unit. (Like
        // when the destructor is inline in the header).

        // We try a best-effort search. If we still don't see releases, we
        // proceed to recognize it as a pointer / observer.

        // Theoretically it's not hard to imagine adversarial code breaking
        // this. In those cases we'll need some fancy schmancy cross-TU trick.
        // But we seem to be able to get away with this heuristic in ORCA.
        if (!match(releaseCallExpr(reference_to_field), ast_context).empty())
          continue;
      }

      AnnotateFieldPointer(field_decl);
    }

    // N.B. we don't need to use the fully qualified name
    // gpos::CRefCount::SafeRelease because
    // 1. unqualified name matching is much faster
    // 2. this leaves room for a CRTP implementation in the future
    // 3. But hopefully with the introduction of smart pointers, SafeRelease
    // will disappear...
    for (const auto& bound_nodes :
         match(releaseCallExpr(
                   declRefExpr(to(varDecl().bind("owner_var")),
                               unless(forFunction(hasName("SafeRelease"))))),
               ast_context)) {
      const auto* owner_var =
          bound_nodes.getNodeAs<clang::VarDecl>("owner_var");
      if (const auto* owner_parm =
              llvm::dyn_cast<clang::ParmVarDecl>(owner_var)) {
        auto parameter_index = owner_parm->getFunctionScopeIndex();

        const auto* function = llvm::cast<clang::FunctionDecl>(
            owner_parm->getParentFunctionOrMethod());
        AnnotateParameterOwner(function, parameter_index);

        if (!match(cxxMethodDecl(isOverride()), *function, ast_context)
                 .empty()) {
          const auto* method = llvm::cast<clang::CXXMethodDecl>(function);
          for (const auto* super_method : method->overridden_methods()) {
            const auto* parm = super_method->getParamDecl(parameter_index);
            AnnotateParameterOwner(super_method, parameter_index);
          }
        }
      } else {
        AnnotateVarOwner(owner_var);
      }
    }

    auto refcount_var = varDecl(hasType(RefCountPointerType()));

    for (const auto& bound_nodes :
         match(varDecl(refcount_var.bind("owner_var"),
                       hasInitializer(ignoringParenImpCasts(cxxNewExpr()))),
               ast_context)) {
      const auto* owner_var =
          bound_nodes.getNodeAs<clang::VarDecl>("owner_var");

      AnnotateVarOwner(owner_var);
    }

    for (const auto& bound_nodes :
         match(binaryOperator(
                   hasOperatorName("="),
                   hasOperands(declRefExpr(to(refcount_var.bind("owner_var"))),
                               ignoringParenImpCasts(cxxNewExpr()))),
               ast_context)) {
      const auto* owner_var =
          bound_nodes.getNodeAs<clang::VarDecl>("owner_var");

      AnnotateVarOwner(owner_var);
    }

    for (const auto& bound_nodes :
         match(functionDecl(returns(RefCountPointerType()),
                            hasDescendant(returnStmt(hasReturnValue(
                                ignoringParenImpCasts(cxxNewExpr())))))
                   .bind("f"),
               ast_context)) {
      const auto* f = bound_nodes.getNodeAs<clang::FunctionDecl>("f");

      AnnotateFunctionReturnOwner(f);
    }

    for (const auto& bound_nodes : match(
             cxxMethodDecl(
                 hasDescendant(returnStmt(hasReturnValue(ignoringParenImpCasts(
                     FieldReferenceFor(fieldDecl(hasType(RefCountPointerType()))
                                           .bind("field")))))),
                 unless(hasDescendant(stmt(
                     anyOf(cxxMemberCallExpr(
                               callee(cxxMethodDecl(hasName("AddRef"))),
                               on(FieldReferenceFor(equalsBoundNode("field")))),
                           binaryOperator(hasOperatorName("="),
                                          hasLHS(FieldReferenceFor(
                                              equalsBoundNode("field")))))))))
                 .bind("f"),
             ast_context)) {
      const auto* f = bound_nodes.getNodeAs<clang::CXXMethodDecl>("f");
      AnnotateFunctionReturnPointer(f);
    }
  }

  void AnnotateParameterOwner(const clang::FunctionDecl* function,
                              unsigned int parameter_index) const {
    for (const auto* f = function; f; f = f->getPreviousDecl()) {
      const auto* parm = f->getParamDecl(parameter_index);
      AnnotateVarOwner(parm);
    }
  }

  void AnnotateVarOwner(const clang::VarDecl* var) const {
    if (!match(varDecl(hasType(OwnerType())), *var, ast_context).empty())
      return;

    AnnotateVar(var, kOwnerAnnotation);
  }

  void PropagateVirtualFunctionReturnType(
      const decltype(OwnerType())& annotation_matcher,
      const char* const annotation) const {
    for (const auto& bound_nodes : match(
             cxxMethodDecl(
                 isOverride(),
                 anyOf(cxxMethodDecl(returns(annotation_matcher),
                                     forEachOverridden(
                                         cxxMethodDecl(unless(returns(
                                                           annotation_matcher)))
                                             .bind("follow"))),
                       cxxMethodDecl(
                           unless(returns(annotation_matcher)),
                           forEachOverridden(returns(annotation_matcher)))
                           .bind("follow"))),
             ast_context)) {
      const auto* m = bound_nodes.getNodeAs<clang::CXXMethodDecl>("follow");

      AnnotateFunctionReturnType(m, annotation_matcher, annotation);
    }
  }

  void AnnotateFunctionReturnType(const clang::FunctionDecl* f,
                                  decltype(OwnerType())
                                      const& annotation_matcher,
                                  const char* annotation) const {
    for (; f; f = f->getPreviousDecl()) {
      auto rt = f->getReturnType();
      if (!match(annotation_matcher, rt, ast_context).empty()) continue;
      AnnotateFunctionReturnType(f, annotation);
    }
  }

  void AnnotateFunctionReturnOwner(const clang::FunctionDecl* f) const {
    AnnotateFunctionReturnType(f, OwnerType(), kOwnerAnnotation);
  }

  void AnnotateFunctionReturnPointer(const clang::FunctionDecl* f) const {
    AnnotateFunctionReturnType(f, PointerType(), kPointerAnnotation);
  }

  void AnnotateFunctionReturnType(const clang::FunctionDecl* f,
                                  const char* annotation) const {
    auto rt = f->getReturnType();
    auto rt_range = f->getReturnTypeSourceRange();
    if (rt->getPointeeType().isConstQualified()) {
      FindConstTokenBefore(f->getSourceRange().getBegin(), rt_range);
    }
    AnnotateSourceRange(rt_range, annotation);
  }

  void FindConstTokenBefore(clang::SourceLocation begin_loc,
                            clang::SourceRange& rt_range) const {
    auto end_loc = rt_range.getEnd();
    auto [file_id, offset] = source_manager.getDecomposedLoc(begin_loc);
    auto start_of_file = source_manager.getLocForStartOfFile(file_id);
    clang::Lexer raw_lexer(start_of_file, lang_opts,
                           source_manager.getCharacterData(start_of_file),
                           source_manager.getCharacterData(begin_loc),
                           source_manager.getCharacterData(end_loc));
    clang::Token token;
    while (!raw_lexer.LexFromRawLexer(token)) {
      if (!token.is(clang::tok::raw_identifier)) continue;
      auto& identifier_info = ast_context.Idents.get(
          llvm::StringRef(source_manager.getCharacterData(token.getLocation()),
                          token.getLength()));
      token.setIdentifierInfo(&identifier_info);
      token.setKind(identifier_info.getTokenID());

      if (!token.is(clang::tok::kw_const)) continue;
      if (source_manager.isBeforeInTranslationUnit(token.getLocation(),
                                                   rt_range.getBegin())) {
        rt_range.setBegin(token.getLocation());
        break;
      }
    }
  }

  void AnnotateVar(const clang::VarDecl* var,
                   llvm::StringRef annotation) const {
    auto source_range = var->getTypeSourceInfo()->getTypeLoc().getSourceRange();

    AnnotateSourceRange(source_range, annotation);
  }

  void AnnotateSourceRange(clang::SourceRange source_range,
                           const llvm::StringRef& annotation) const {
    auto type_text = clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(source_range), source_manager,
        lang_opts);

    std::string new_text = (annotation + "<" + type_text + ">").str();

    clang::tooling::Replacement replacement(
        source_manager, clang::CharSourceRange::getTokenRange(source_range),
        new_text, lang_opts);
    std::string file_path = replacement.getFilePath().str();
    llvm::cantFail(replacements[file_path].add(replacement));
  }

  void AnnotateFieldOwner(const clang::FieldDecl* field) const {
    if (!match(fieldDecl(hasType(OwnerType())), *field, ast_context).empty())
      return;
    AnnotateField(field, kOwnerAnnotation);
  }

  void AnnotateFieldPointer(const clang::FieldDecl* field) const {
    if (!match(fieldDecl(hasType(PointerType())), *field, ast_context).empty())
      return;
    AnnotateField(field, kPointerAnnotation);
  }

  void AnnotateField(const clang::FieldDecl* field_decl,
                     llvm::StringRef annotation) const {
    auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();
    clang::SourceRange type_range = field_type_loc.getSourceRange();
    auto field_qual_type = field_decl->getType();
    auto pointee_type = field_qual_type->getPointeeType();

    const char* opt_mutable = field_decl->isMutable() ? "mutable " : "";
    std::string pointee_cv;
    auto pointee_local_qualifiers = pointee_type.getLocalQualifiers();
    if (pointee_local_qualifiers.hasConst()) {
      pointee_cv = "const ";
    }
    if (pointee_local_qualifiers.hasVolatile()) {
      pointee_cv += "volatile ";
    }
    auto field_type_text = clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(type_range), source_manager,
        lang_opts);
    std::string new_text =
        (opt_mutable + annotation + "<" + pointee_cv + field_type_text + ">")
            .str();

    // HACK: notice that the replacement range isn't just the type but it also
    // extends to the beginning of the declarator. This is so that we cover the
    // cases of "const mutable T*" or "mutable const volatile T*"
    clang::tooling::Replacement annotation_rep(
        source_manager,
        clang::CharSourceRange::getTokenRange(field_decl->getBeginLoc(),
                                              field_type_loc.getEndLoc()),
        new_text, lang_opts);
    std::string file_path = annotation_rep.getFilePath().str();
    llvm::cantFail(replacements[file_path].add(annotation_rep));
  }
};

class AnnotateASTConsumer : public clang::ASTConsumer {
  FileToReplacements& replacements_;
  ActionOptions action_options_;

 public:
  explicit AnnotateASTConsumer(FileToReplacements& replacements,
                               ActionOptions action_options)
      : replacements_(replacements), action_options_(action_options) {}

  void HandleTranslationUnit(clang::ASTContext& ast_context) override {
    Annotator annotator{action_options_, replacements_, ast_context,
                        ast_context.getSourceManager(),
                        ast_context.getLangOpts()};

    annotator.Annotate();
  }
};

std::unique_ptr<clang::ASTConsumer> AnnotateAction::newASTConsumer() {
  return std::make_unique<AnnotateASTConsumer>(replacements_, action_options_);
}

}  // namespace orca_tidy
