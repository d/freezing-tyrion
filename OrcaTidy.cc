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

static auto FieldReferenceFor(DeclarationMatcher const& field_matcher) {
  return memberExpr(member(field_matcher),
                    hasObjectExpression(ignoringParenImpCasts(cxxThisExpr())));
}

using ExpressionMatcher = decltype(ignoringParenImpCasts(anything()));

static auto ReleaseCallExpr(ExpressionMatcher const& reference_to_field) {
  auto release = cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("Release"), parameterCountIs(0))),
      on(reference_to_field));
  auto safe_release = callExpr(
      callee(functionDecl(hasName("SafeRelease"), parameterCountIs(1))),
      hasArgument(0, reference_to_field));
  return callExpr(anyOf(release, safe_release));
};

static auto AddRefOn(ExpressionMatcher const& expr_matcher) {
  return cxxMemberCallExpr(callee(cxxMethodDecl(hasName("AddRef"))),
                           on(expr_matcher));
}

using VarSet = llvm::DenseSet<const clang::VarDecl*>;
AST_MATCHER_P_OVERLOAD(clang::VarDecl, IsInSet, VarSet, nodes, 0) {
  return nodes.contains(&Node);
}

using FieldSet = llvm::DenseSet<const clang::FieldDecl*>;
AST_MATCHER_P_OVERLOAD(clang::FieldDecl, IsInSet, FieldSet, nodes, 1) {
  return nodes.contains(&Node);
}

using CXXMethodMatcher = decltype(isOverride());
AST_MATCHER_P(clang::CXXMethodDecl, HasOverridden, CXXMethodMatcher,
              inner_matcher) {
  return llvm::any_of(Node.overridden_methods(),
                      [=](const clang::CXXMethodDecl* m) {
                        return inner_matcher.matches(*m, Finder, Builder);
                      });
}

AST_MATCHER_P2(clang::CompoundStmt, HasBoundStmtImmediatelyFollowing,
               std::string, id, StatementMatcher, lhs) {
  if (Node.size() < 2) return false;
  StatementMatcher equals_bound_node = equalsBoundNode(id);

  auto prev_range =
      llvm::make_range(std::next(Node.body_rbegin()), Node.body_rend());
  for (auto [prev_stmt, stmt] :
       llvm::zip_first(prev_range, llvm::reverse(Node.body()))) {
    auto builder = *Builder;
    if (equals_bound_node.matches(*stmt, Finder, &builder) &&
        lhs.matches(*prev_stmt, Finder, &builder)) {
      *Builder = std::move(builder);
      return true;
    }
  }
  return false;
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
  void Propagate() const;

  void AnnotateBaseCases() const;

  template <class Node, class Matcher>
  llvm::SmallVector<const Node*> NodesFromMatch(Matcher matcher,
                                                llvm::StringRef id) const {
    llvm::SmallVector<const Node*> nodes;

    llvm::transform(match(matcher, ast_context), std::back_inserter(nodes),
                    [id](clang::ast_matchers::BoundNodes const& bound_nodes) {
                      return bound_nodes.template getNodeAs<Node>(id);
                    });

    return nodes;
  }

  template <class NodeSet, class Matcher>
  NodeSet NodeSetFromMatch(Matcher matcher, llvm::StringRef id) const {
    using Node = std::remove_const_t<
        std::remove_pointer_t<typename NodeSet::value_type>>;
    auto nodes_from_match = NodesFromMatch<Node>(matcher, id);
    NodeSet node_set{nodes_from_match.begin(), nodes_from_match.end()};
    return node_set;
  }

  template <class Matcher>
  VarSet VarSetFromMatch(Matcher matcher, llvm::StringRef id) const {
    return NodeSetFromMatch<VarSet>(matcher, id);
  }

  template <class Matcher>
  FieldSet FieldSetFromMatch(Matcher matcher, llvm::StringRef id) const {
    return NodeSetFromMatch<FieldSet>(matcher, id);
  }

  auto RefCountVarInitializedOrAssigned(
      ExpressionMatcher const& init_expr_matcher) const {
    // We're not quite ready to handle multiple-declaration yet, so here's a
    // best effort to walk (carefully) around them. Amazingly, this doesn't seem
    // to disrupt any of the base cases.

    auto refcount_var =
        varDecl(hasType(RefCountPointerType()),
                unless(hasParent(declStmt(unless(declCountIs(1))))));

    return varDecl(anyOf(
        allOf(refcount_var,
              hasInitializer(ignoringParenImpCasts(init_expr_matcher))),
        IsInSet(VarSetFromMatch(
            binaryOperator(
                hasOperatorName("="),
                hasOperands(declRefExpr(to(refcount_var.bind("owner_var"))),
                            ignoringParenImpCasts(init_expr_matcher))),
            "owner_var"))));
  }

  auto FieldReleased() const {
    return IsInSet(FieldSetFromMatch(
        ReleaseCallExpr(FieldReferenceFor(fieldDecl().bind("owner_field"))),
        "owner_field"));
  }

  void AnnotateParameterOwner(const clang::FunctionDecl* function,
                              unsigned int parameter_index) const {
    for (const auto* f : function->redecls()) {
      const auto* parm = f->getParamDecl(parameter_index);
      AnnotateVarOwner(parm);
    }
  }

  void AnnotateVarOwner(const clang::VarDecl* var) const {
    for (const auto* v : var->redecls()) {
      AnnotateVar(v, OwnerType(), kOwnerAnnotation);
    }
  }

  void PropagateVirtualFunctionReturnType(const TypeMatcher& annotation_matcher,
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

  void AnnotateFunctionReturnType(const clang::FunctionDecl* function,
                                  const TypeMatcher& annotation_matcher,
                                  const char* annotation) const {
    for (const auto* f : function->redecls()) {
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
                   const TypeMatcher& annotation_matcher,
                   llvm::StringRef annotation) const {
    if (!match(annotation_matcher, var->getType(), ast_context).empty()) return;

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
    AnnotateField(field, OwnerType(), kOwnerAnnotation);
  }

  void AnnotateFieldPointer(const clang::FieldDecl* field) const {
    AnnotateField(field, PointerType(), kPointerAnnotation);
  }

  void AnnotateField(const clang::FieldDecl* field_decl,
                     const TypeMatcher& annotation_matcher,
                     llvm::StringRef annotation) const {
    if (!match(annotation_matcher, field_decl->getType(), ast_context).empty())
      return;

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

void Annotator::Propagate() const {
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(
               returns(qualType(unless(OwnerType()), RefCountPointerType())),
               hasDescendant(returnStmt(hasReturnValue(ignoringParenImpCasts(
                   anyOf(declRefExpr(to(varDecl(hasType(OwnerType())))),
                         callExpr(
                             callee(functionDecl(returns(OwnerType()))))))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }

  PropagateVirtualFunctionReturnType(OwnerType(), kOwnerAnnotation);
  PropagateVirtualFunctionReturnType(PointerType(), kPointerAnnotation);

  for (const auto* derived_method : NodesFromMatch<clang::CXXMethodDecl>(
           cxxMethodDecl(isOverride(),
                         HasOverridden(hasAnyParameter(hasType(OwnerType()))))
               .bind("derived"),
           "derived")) {
    for (const auto* base_method : derived_method->overridden_methods()) {
      for (const auto* base_param : base_method->parameters()) {
        if (match(OwnerType(), base_param->getType(), ast_context).empty())
          continue;
        auto parameter_index = base_param->getFunctionScopeIndex();

        AnnotateParameterOwner(derived_method, parameter_index);
      }
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

  for (const auto* method : NodesFromMatch<clang::CXXMethodDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(FieldReferenceFor(
                   fieldDecl(hasType(OwnerType())).bind("field")))),
               forFunction(cxxMethodDecl(hasDescendant(binaryOperator(
                                             hasLHS(FieldReferenceFor(
                                                 equalsBoundNode("field"))),
                                             hasOperatorName("="))))
                               .bind("method"))),
           "method")) {
    AnnotateFunctionReturnPointer(method);
  }

  for (const auto* method : NodesFromMatch<clang::CXXMethodDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(FieldReferenceFor(
                   fieldDecl(hasType(PointerType())).bind("field")))),
               forFunction(
                   cxxMethodDecl(
                       unless(hasDescendant(stmt(anyOf(
                           AddRefOn(
                               FieldReferenceFor(equalsBoundNode("field"))),
                           binaryOperator(
                               hasOperatorName("="),
                               hasOperands(
                                   FieldReferenceFor(equalsBoundNode("field")),
                                   callExpr(callee(functionDecl(
                                       returns(OwnerType())))))))))))
                       .bind("method"))),
           "method")) {
    AnnotateFunctionReturnPointer(method);
  }

  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(unless(hasType(OwnerType())),
                   RefCountVarInitializedOrAssigned(
                       callExpr(callee(functionDecl(returns(OwnerType()))))))
               .bind("owner_var"),
           "owner_var")) {
    AnnotateVarOwner(var);
  }
}

void Annotator::AnnotateBaseCases() const {
  auto field_is_released = FieldReleased();
  for (const auto* field : NodesFromMatch<clang::FieldDecl>(
           fieldDecl(field_is_released).bind("owner_field"), "owner_field")) {
    AnnotateFieldOwner(field);
  }

  // Intuitively, a field that isn't released anywhere is a pointer.
  // Challenge: how do we know we've seen "everywhere" we can find?
  // Heuristics:
  // 1. If a class has no destructor, give up and assume the field in question
  // is a pointer.
  // 2. If a class has a destructor, but its definition isn't visible in the
  // current translation unit, assume we haven't seen the main file yet, and
  // punt.
  // 3. Otherwise, we're either processing the main file, or we've seen the
  // definition of the destructor. In either case, we hope it's good enough for
  // us to see a Release() if there's any.
  auto has_destructor_in_translation_unit =
      hasMethod(cxxDestructorDecl(anyOf(hasAnyBody(stmt()), isDefaulted())));
  auto has_no_destructor = unless(hasMethod(cxxDestructorDecl()));
  for (const auto* field_decl : NodesFromMatch<clang::FieldDecl>(
           fieldDecl(unless(hasType(PointerType())),
                     hasType(RefCountPointerType()),
                     anyOf(hasDeclContext(cxxRecordDecl(has_no_destructor)),
                           allOf(hasDeclContext(cxxRecordDecl(
                                     has_destructor_in_translation_unit)),
                                 unless(field_is_released))))
               .bind("field"),
           "field")) {
    AnnotateFieldPointer(field_decl);
  }

  // N.B. we don't need to use the fully qualified name
  // gpos::CRefCount::SafeRelease because
  // 1. unqualified name matching is much faster
  // 2. this leaves room for a CRTP implementation in the future
  // 3. But hopefully with the introduction of smart pointers, SafeRelease
  // will disappear...
  for (const auto* owner_var : NodesFromMatch<clang::VarDecl>(
           ReleaseCallExpr(
               declRefExpr(to(varDecl().bind("owner_var")),
                           unless(forFunction(hasName("SafeRelease"))))),
           "owner_var")) {
    if (const auto* owner_parm =
            llvm::dyn_cast<clang::ParmVarDecl>(owner_var)) {
      auto parameter_index = owner_parm->getFunctionScopeIndex();

      const auto* function = llvm::cast<clang::FunctionDecl>(
          owner_parm->getParentFunctionOrMethod());
      AnnotateParameterOwner(function, parameter_index);

      if (!match(cxxMethodDecl(isOverride()), *function, ast_context).empty()) {
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

  for (const auto* owner_var : NodesFromMatch<clang::VarDecl>(
           varDecl(varDecl().bind("owner_var"),
                   RefCountVarInitializedOrAssigned(cxxNewExpr())),
           "owner_var")) {
    AnnotateVarOwner(owner_var);
  }

  for (const auto* v : NodesFromMatch<clang::VarDecl>(
           returnStmt(returnStmt().bind("return"),
                      hasReturnValue(ignoringParenImpCasts(declRefExpr(
                          to(varDecl(hasLocalStorage()).bind("var"))))),
                      hasParent(compoundStmt(HasBoundStmtImmediatelyFollowing(
                          "return",
                          AddRefOn(declRefExpr(to(equalsBoundNode("var")))))))),
           "var")) {
    AnnotateVar(v, PointerType(), kPointerAnnotation);
  }

  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(returns(RefCountPointerType()),
                        hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(cxxNewExpr())))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }

  for (const auto* f : NodesFromMatch<clang::CXXMethodDecl>(
           cxxMethodDecl(
               hasDescendant(returnStmt(hasReturnValue(ignoringParenImpCasts(
                   FieldReferenceFor(fieldDecl(hasType(RefCountPointerType()))
                                         .bind("field")))))),
               unless(hasDescendant(stmt(anyOf(
                   AddRefOn(FieldReferenceFor(equalsBoundNode("field"))),
                   binaryOperator(
                       hasOperatorName("="),
                       hasLHS(FieldReferenceFor(equalsBoundNode("field")))))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnPointer(f);
  }
}

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
