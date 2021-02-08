#include "OrcaTidy.h"
#include "AstHelpers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Tooling.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;
namespace tooling = clang::tooling;

using FileToReplacements = std::map<std::string, tooling::Replacements>;

__attribute__((const)) static StatementMatcher CallReturningOwner() {
  return callExpr(
      anyOf(hasType(OwnerType()), CallCcacheAccessorMethodsReturningOwner()));
}

static auto FieldReferenceFor(DeclarationMatcher const& field_matcher) {
  return memberExpr(member(field_matcher),
                    hasObjectExpression(ignoringParenImpCasts(cxxThisExpr())));
}

static auto ReleaseCallExpr(ExpressionMatcher const& reference_to_field) {
  auto release = cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("Release"), parameterCountIs(0))),
      on(reference_to_field));
  auto safe_release = callExpr(
      callee(functionDecl(hasName("SafeRelease"), parameterCountIs(1))),
      hasArgument(0, reference_to_field));
  return callExpr(anyOf(release, safe_release));
}

static auto AddRefOn(ExpressionMatcher const& expr_matcher) {
  return cxxMemberCallExpr(callee(cxxMethodDecl(hasName("AddRef"))),
                           on(expr_matcher));
}

AST_MATCHER(clang::NamedDecl, Unnamed) { return !Node.getDeclName(); }

AST_MATCHER(clang::NamedDecl, Unused) {
  return Node.hasAttr<clang::UnusedAttr>() || !Node.getDeclName() ||
         !Node.isReferenced();
}

using CXXMethodMatcher = decltype(isOverride());
AST_MATCHER_P(clang::CXXMethodDecl, HasOverridden, CXXMethodMatcher,
              inner_matcher) {
  return llvm::any_of(Node.overridden_methods(),
                      [=](const clang::CXXMethodDecl* m) {
                        return inner_matcher.matches(*m, Finder, Builder);
                      });
}

AST_MATCHER(clang::FunctionDecl, HasRedecls) {
  return Node.getFirstDecl() != Node.getMostRecentDecl();
}

template <class Parent, class Node>
const Parent* GetParentAs(const Node& node, clang::ASTContext& ast_context) {
  for (auto potential_parent : ast_context.getParents(node))
    if (const auto* parent = potential_parent.template get<Parent>(); parent)
      return parent;
  return nullptr;
}

AST_MATCHER_P(clang::Stmt, StmtIsImmediatelyBefore, StatementMatcher, rhs) {
  const auto* compound_stmt =
      GetParentAs<clang::CompoundStmt>(Node, Finder->getASTContext());
  if (!compound_stmt) return false;
  const auto* node_it = llvm::find(compound_stmt->body(), &Node);
  const auto* rhs_it = std::next(node_it);
  return rhs_it != compound_stmt->body_end() &&
         rhs.matches(**rhs_it, Finder, Builder);
}

AST_MATCHER_P(clang::Stmt, StmtIsImmediatelyAfter, StatementMatcher, lhs) {
  const auto* compound_stmt =
      GetParentAs<clang::CompoundStmt>(Node, Finder->getASTContext());
  if (!compound_stmt) return false;
  const auto* node_it = llvm::find(compound_stmt->body(), &Node);
  const auto* lhs_it = std::prev(node_it);
  return node_it != compound_stmt->body_begin() &&
         lhs.matches(**lhs_it, Finder, Builder);
}

AST_MATCHER_P(clang::RecordDecl, HasField, DeclarationMatcher, field_matcher) {
  return matchesFirstInPointerRange(field_matcher, Node.field_begin(),
                                    Node.field_end(), Finder,
                                    Builder) != Node.field_end();
}

using ReturnMatcher = decltype(hasReturnValue(expr()));
static ReturnMatcher ReturnAfterAddRef(const ExpressionMatcher& retval,
                                       const ExpressionMatcher& addref_ref) {
  return returnStmt(hasReturnValue(ignoringParenCasts(retval)),
                    StmtIsImmediatelyAfter(AddRefOn(addref_ref)));
}

StatementMatcher AddRefOrAssign(const ExpressionMatcher expr) {
  return anyOf(AddRefOn(expr), AssignTo(expr));
}

static TypeMatcher TypeContainingTypedefFunctionPointer(
    const DeclarationMatcher& typedef_matcher) {
  auto typedef_or_points_to_typedef = qualType(anyOf(
      hasDeclaration(typedefNameDecl(
          hasType(hasCanonicalType(
              anyOf(pointsTo(functionProtoType()),
                    memberPointerType(pointee(functionProtoType()))))),
          typedef_matcher)),
      pointsTo(typedefNameDecl(hasType(hasCanonicalType(functionProtoType())),
                               typedef_matcher))));
  return qualType(anyOf(
      typedef_or_points_to_typedef,
      arrayType(hasElementType(hasDeclaration(recordDecl(
          HasField(fieldDecl(hasType(typedef_or_points_to_typedef)))))))));
}

using ProtoTypeMatcher = decltype(functionProtoType().bind(""));
static ExpressionMatcher ExprInitializingFunctionPointer(
    const ProtoTypeMatcher& fproto_matcher) {
  return anyOf(
      expr(hasType(qualType(anyOf(pointsTo(fproto_matcher), fproto_matcher)))),
      initListExpr(hasDescendant(initListExpr(has(expr(hasType(
          qualType(anyOf(pointsTo(fproto_matcher),
                         memberPointerType(pointee(fproto_matcher)))))))))));
}

static StatementMatcher VarInitWithTypedefFunctionPointers(
    const DeclarationMatcher& typedef_matcher,
    const ProtoTypeMatcher& fproto_matcher) {
  return declStmt(forEach(varDecl(
      hasType(TypeContainingTypedefFunctionPointer(typedef_matcher)),
      hasInitializer(ExprInitializingFunctionPointer(fproto_matcher)))));
}

static StatementMatcher CallPassingFunctionToTypedefFunctionPointer(
    const DeclarationMatcher& typedef_matcher,
    const ProtoTypeMatcher& fproto_matcher) {
  return callExpr(forEachArgumentWithParamType(
      ExprInitializingFunctionPointer(fproto_matcher),
      TypeContainingTypedefFunctionPointer(typedef_matcher)));
}

/// Whenever we think of using \c forEachArgumentWithParam or
/// \c forEachArgumentWithParamType with \c callExpr, we probably should also
/// consider using them with \c cxxConstructExpr . That's what these two
/// matchers, \c NonTemplateCallOrConstruct, and \c CallOrConstruct are for:
/// they will match either a \c CallExpr or a \c CXXConstructExpr with all your
/// passed in matchers.
/// In addition, we should consider the non-template version when we intend to
/// infer the parameter annotations for the callee, because function templates
/// (or methods of class templates) can exhibit different owning behavior
/// depending on the instantiation (c.f. \c CDynamicPtrArrary::Append)
template <class... Matchers>
static auto NonTemplateCallOrConstruct(Matchers... matchers) {
  auto non_template =
      unless(hasDeclaration(functionDecl(isTemplateInstantiation())));
  return expr(anyOf(callExpr(non_template, matchers...),
                    cxxConstructExpr(non_template, matchers...)));
}

template <class... Matchers>
static auto CallOrConstruct(Matchers... matchers) {
  return expr(anyOf(callExpr(matchers...), cxxConstructExpr(matchers...)));
}

static StatementMatcher PassedAsArgumentToNonPointerParam(
    const DeclarationMatcher& var_matcher) {
  auto ref_to_var = declRefExpr(to(var_matcher));
  return anyOf(
      CallOrConstruct(
          forEachArgumentWithParamType(ref_to_var, unless(PointerType()))),
      parenListExpr(has(ref_to_var)), initListExpr(has(ref_to_var)),
      callExpr(
          callee(expr(anyOf(unresolvedLookupExpr(), unresolvedMemberExpr()))),
          hasAnyArgument(ignoringParenCasts(ref_to_var))));
}

static StatementMatcher InitializingOrAssigningWith(
    const ExpressionMatcher& expr) {
  return anyOf(declStmt(has(varDecl(hasInitializer(ignoringParenCasts(expr))))),
               AssignTo(anything(), ignoringParenCasts(expr)));
}

AST_MATCHER(clang::VarDecl, IsImmediatelyBeforeAddRef) {
  return varDecl(hasParent(declStmt(StmtIsImmediatelyBefore(
                     AddRefOn(declRefExpr(to(equalsNode(&Node))))))))
      .matches(Node, Finder, Builder);
}

struct Annotator : NodesFromMatchBase<Annotator> {
  ActionOptions action_options;
  FileToReplacements& replacements;
  clang::ASTContext& ast_context;
  const clang::SourceManager& source_manager;
  const clang::LangOptions& lang_opts;

  Annotator(const ActionOptions& action_options,
            FileToReplacements& replacements, clang::ASTContext& ast_context,
            const clang::SourceManager& source_manager,
            const clang::LangOptions& lang_opts);
  clang::ASTContext& AstContext() const;

  void Annotate() {
    if (action_options.Base) AnnotateBaseCases();

    if (action_options.Propagate) Propagate();
  }

 private:
  void Propagate() const;

  void AnnotateBaseCases() const;

  auto FieldReleased() const {
    return IsInSet(DeclSetFromMatch(
        ReleaseCallExpr(FieldReferenceFor(fieldDecl().bind("owner_field"))),
        "owner_field"));
  }

  void AnnotateFunctionParameterOwner(const clang::FunctionDecl* function,
                                      unsigned int parameter_index) const {
    AnnotateFunctionParameter(function, parameter_index, OwnerType(),
                              kOwnerAnnotation);
  }

  void AnnotateFunctionParameterPointer(const clang::FunctionDecl* function,
                                        unsigned int parameter_index) const {
    AnnotateFunctionParameter(function, parameter_index, PointerType(),
                              kPointerAnnotation);
  }

  void AnnotateFunctionParameter(const clang::FunctionDecl* function,
                                 unsigned int parameter_index,
                                 const TypeMatcher& annotation_matcher,
                                 llvm::StringRef annotation) const {
    for (const auto* f : function->redecls()) {
      const auto* parm = f->getParamDecl(parameter_index);
      AnnotateOneVar(parm, annotation_matcher, annotation);
    }
  }

  void PropagateVirtualFunctionReturnTypes() const {
    for (auto [m, rt] : NodesFromMatch<clang::CXXMethodDecl, clang::Type>(
             cxxMethodDecl(
                 isOverride(),
                 anyOf(
                     cxxMethodDecl(
                         returns(qualType(AnnotatedType(), type().bind("rt"))),
                         forEachOverridden(cxxMethodDecl().bind("follow"))),
                     cxxMethodDecl(HasOverridden(returns(qualType(
                                       AnnotatedType(), type().bind("rt")))))
                         .bind("follow"))),
             "follow", "rt")) {
      if (IsOwner(rt))
        AnnotateFunctionReturnType(m, OwnerType(), kOwnerAnnotation);
      else if (IsPointer(rt))
        AnnotateFunctionReturnType(m, PointerType(), kPointerAnnotation);
    }
  }

  void AnnotateFunctionReturnType(const clang::FunctionDecl* function,
                                  const TypeMatcher& annotation_matcher,
                                  llvm::StringRef annotation) const {
    for (const auto* f : function->redecls()) {
      auto rt = f->getReturnType();
      if (Match(annotation_matcher, rt)) continue;
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
                                  llvm::StringRef annotation) const {
    auto rt = f->getReturnType();
    auto rt_range = f->getReturnTypeSourceRange();
    if (rt->getPointeeType().isLocalConstQualified()) {
      FindConstTokenBefore(f->getBeginLoc(), rt_range);
    }
    AnnotateSourceRange(rt_range, annotation);
  }

  void AnnotateTypedefFunctionProtoTypeReturnPointer(
      const clang::TypedefNameDecl* typedef_name_decl) const;
  void AnnotateTypedefFunctionProtoTypeReturnType(
      const clang::TypedefNameDecl* typedef_decl,
      const TypeMatcher& annotation_matcher, llvm::StringRef annotation) const;

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

  void AnnotateOneVar(const clang::VarDecl* var,
                      const TypeMatcher& annotation_matcher,
                      llvm::StringRef annotation) const {
    for (const auto* v : var->redecls()) {
      if (Match(annotation_matcher, v->getType())) continue;
      if (IsLeaked(var)) continue;

      auto source_range = v->getTypeSourceInfo()->getTypeLoc().getSourceRange();
      if (v->getType()->getPointeeType().isLocalConstQualified() &&
          !v->getType()->isTypedefNameType()) {
        FindConstTokenBefore(v->getBeginLoc(), source_range);
      }
      AnnotateSourceRange(source_range, annotation);
    }
  }

  void AnnotateSourceRange(clang::SourceRange source_range,
                           llvm::StringRef annotation) const {
    orca_tidy::AnnotateSourceRange(source_range, annotation, ast_context,
                                   replacements);
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
    if (Match(annotation_matcher, field_decl->getType())) return;

    auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();
    auto pointer_loc = IgnoringElaboratedQualified(field_type_loc)
                           .getAs<clang::PointerTypeLoc>();
    // While the underlying type is a pointer type, the source code isn't
    // spelled out in a pointer form (i.e. "T*". e.g. a typedef, or a
    // template type parameter substitution). Annotating such things will be
    // problematic.
    if (!pointer_loc) return;

    auto pointee_loc = pointer_loc.getPointeeLoc();
    clang::SourceRange type_range = field_type_loc.getSourceRange();
    auto pointee_type = pointee_loc.getType();

    if (pointee_type.isLocalConstQualified())
      FindConstTokenBefore(field_decl->getBeginLoc(), type_range);

    AnnotateSourceRange(type_range, annotation);
  }

  template <class Matcher, class Node>
  bool Match(Matcher matcher, const Node& node) const {
    return !match(matcher, node, ast_context).empty();
  }

  bool IsOwner(const clang::QualType& type) const {
    return Match(OwnerType(), type);
  }

  bool IsPointer(const clang::QualType& type) const {
    return Match(PointerType(), type);
  }

  bool IsLeaked(const clang::VarDecl* var) const {
    return Match(LeakedType(), var->getType());
  }

  bool IsAnnotated(const clang::QualType& type) const {
    return Match(AnnotatedType(), type);
  }

  bool IsOwner(const clang::Type* type) const {
    return Match(elaboratedType(namesType(OwnerType())), *type);
  }

  bool IsPointer(const clang::Type* type) const {
    return Match(elaboratedType(namesType(PointerType())), *type);
  }

  void AnnotateParameter(const clang::ParmVarDecl* p,
                         const TypeMatcher& annotation_matcher,
                         llvm::StringRef annotation) const;
  void AnnotateVarOwner(const clang::VarDecl* var) const;
  void AnnotateVarPointer(const clang::VarDecl* v) const;

  void AnnotateVar(const clang::VarDecl* v,
                   const TypeMatcher& annotation_matcher,
                   llvm::StringRef annotation) const;
  void MoveSourceRange(clang::SourceRange source_range) const;
  void PropagateTailCall() const;
  void PropagateFunctionPointers() const;
  void AnnotateTypedefFunctionProtoTypeReturnOwner(
      const clang::TypedefNameDecl* typedef_decl) const;
  void PropagateReturnOwnerVar() const;
  void InferAddRefReturn() const;
  void InferInitAddRef() const;
  void InferPointerVars() const;
  void InferFields() const;
  void InferGetters() const;
  void InferOwnerVars() const;
  void InferReturnNew() const;
  void PropagatePointerVars() const;
};

void Annotator::Propagate() const {
  PropagatePointerVars();

  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(returns(RefCountPointerType()),
                        hasAnyBody(hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(CallReturningOwner()))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }

  PropagateReturnOwnerVar();

  PropagateVirtualFunctionReturnTypes();

  for (const auto* derived_method : NodesFromMatch<clang::CXXMethodDecl>(
           cxxMethodDecl(isOverride(), HasOverridden(hasAnyParameter(
                                           hasType(AnnotatedType()))))
               .bind("derived"),
           "derived")) {
    for (const auto* base_method : derived_method->overridden_methods()) {
      for (const auto* base_param : base_method->parameters()) {
        auto t = base_param->getType();
        auto parameter_index = base_param->getFunctionScopeIndex();

        if (IsOwner(t))
          AnnotateFunctionParameterOwner(derived_method, parameter_index);
        else if (IsPointer(t))
          AnnotateFunctionParameterPointer(derived_method, parameter_index);
      }
    }
  }

  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(HasRedecls(), hasAnyParameter(hasType(AnnotatedType())))
               .bind("f"),
           "f")) {
    for (const auto* p : f->parameters()) {
      auto t = p->getType();
      auto parameter_index = p->getFunctionScopeIndex();
      if (IsOwner(t))
        AnnotateFunctionParameterOwner(f, parameter_index);
      else if (IsPointer(t))
        AnnotateFunctionParameterPointer(f, parameter_index);
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
               forFunction(cxxMethodDecl(hasAnyBody(hasDescendant(
                                             AssignTo(FieldReferenceFor(
                                                 equalsBoundNode("field"))))))
                               .bind("method"))),
           "method")) {
    AnnotateFunctionReturnPointer(method);
  }

  for (const auto* method : NodesFromMatch<clang::CXXMethodDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(FieldReferenceFor(
                   fieldDecl(hasType(PointerType())).bind("field")))),
               forFunction(cxxMethodDecl(unless(hasAnyBody(hasDescendant(stmt(
                                             AddRefOrAssign(FieldReferenceFor(
                                                 equalsBoundNode("field"))))))))
                               .bind("method"))),
           "method")) {
    AnnotateFunctionReturnPointer(method);
  }

  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(RefCountVarInitializedOrAssigned(
                       ignoringParenCasts(CallReturningOwner())))
               .bind("owner_var"),
           "owner_var")) {
    AnnotateVarOwner(var);
  }

  // It's tempting to wrap an \c ignoringParenImpCasts inside \c
  // forEachArgumentWithParam here, but note that \c forEachArgumentWithParam
  // already does that for the first argument
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           NonTemplateCallOrConstruct(forEachArgumentWithParam(
               anyOf(cxxNewExpr(), CallReturningOwner()),
               parmVarDecl(hasType(RefCountPointerType())).bind("param"))),
           "param")) {
    AnnotateParameter(param, OwnerType(), kOwnerAnnotation);
  }

  PropagateTailCall();

  PropagateFunctionPointers();
}

void Annotator::PropagateReturnOwnerVar() const {
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(declRefExpr(to(varDecl(
                   hasLocalStorage(), hasType(OwnerType()), decl().bind("var"),
                   hasDeclContext(
                       functionDecl(unless(isInstantiated()),
                                    hasBody(unless(hasDescendant(
                                        PassedAsArgumentToNonPointerParam(
                                            equalsBoundNode("var"))))))
                           .bind("f")))))))),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }
}

// We can infer a lot from the function call expressions contained in the full
// expression being returned:
//
// 1. If a never-AddRef'd var is the argument to an owner param, the var is an
// owner, and the argument is moved.
void Annotator::PropagateTailCall() const {
  // Similar to \c forEachArgumentWithParam, \c forEachArgumentWithParamType
  // will strip parentheses and casts for the first argument, so resist the
  // temptation to wrap the "arg matcher" in a \c ignoringParenImpCasts
  for (auto [var, arg, r] :
       NodesFromMatch<clang::VarDecl, clang::Expr, clang::ReturnStmt>(
           returnStmt(
               forEachDescendant(CallOrConstruct(forEachArgumentWithParamType(
                   expr(declRefExpr(to(varDecl(
                            hasLocalStorage(), varDecl().bind("var"),
                            hasDeclContext(functionDecl(unless(
                                hasAnyBody(hasDescendant(AddRefOn(declRefExpr(
                                    to(equalsBoundNode("var")))))))))))),
                        expr().bind("arg")),
                   OwnerType()))),
               stmt().bind("r")),
           "var", "arg", "r")) {
    if (Match(
            returnStmt(unless(hasDescendant(CallOrConstruct(hasAnyArgument(
                ignoringParenCasts(expr(unless(equalsNode(arg)),
                                        declRefExpr(to(equalsNode(var)))))))))),
            *r)) {
      AnnotateVarOwner(var);
      MoveSourceRange(arg->getSourceRange());
    }
  }

  for (auto [var, r] : NodesFromMatch<clang::VarDecl, clang::ReturnStmt>(
           returnStmt(
               forEachDescendant(CallOrConstruct(forEachArgumentWithParamType(
                   declRefExpr(
                       to(varDecl(unless(isInstantiated()), hasLocalStorage())
                              .bind("var"))),
                   PointerType()))),
               stmt().bind("r")),
           "var", "r")) {
    if (Match(returnStmt(unless(hasDescendant(
                  PassedAsArgumentToNonPointerParam(equalsNode(var))))),
              *r)) {
      AnnotateVarPointer(var);
    }
  }

  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           returnStmt(forEachDescendant(
               NonTemplateCallOrConstruct(forEachArgumentWithParam(
                   declRefExpr(to(varDecl(
                       hasLocalStorage(), hasType(PointerType()),
                       varDecl().bind("var"),
                       hasDeclContext(functionDecl(hasAnyBody(
                           stmt(unless(hasDescendant(AddRefOrAssign(declRefExpr(
                               to(equalsBoundNode("var"))))))))))))),
                   parmVarDecl(unless(isInstantiated())).bind("param"))))),
           "param")) {
    AnnotateVarPointer(param);
  }

  for (auto [param, var, arg, r] :
       NodesFromMatch<clang::ParmVarDecl, clang::VarDecl, clang::Expr,
                      clang::ReturnStmt>(
           returnStmt(
               forEachDescendant(CallOrConstruct(
                   forEachArgumentWithParam(
                       declRefExpr(
                           to(varDecl(hasLocalStorage(), hasType(OwnerType()))
                                  .bind("var")))
                           .bind("arg"),
                       optionally(parmVarDecl(unless(isInstantiated()))
                                      .bind("param"))),
                   hasDeclaration(functionDecl(unless(hasName("std::move")))))),
               stmt().bind("r")),
           "param", "var", "arg", "r")) {
    if (Match(returnStmt(hasDescendant(
                  declRefExpr(unless(equalsNode(arg)), to(equalsNode(var))))),
              *r))
      continue;

    if (param) AnnotateVarOwner(param);
    MoveSourceRange(arg->getSourceRange());
  }
}

void Annotator::MoveSourceRange(clang::SourceRange source_range) const {
  auto range = clang::CharSourceRange::getTokenRange(source_range);
  auto arg_text = clang::Lexer::getSourceText(range, source_manager, lang_opts);
  std::string new_arg = ("std::move(" + arg_text + ")").str();
  tooling::Replacement replacement(source_manager, range, new_arg, lang_opts);
  llvm::cantFail(
      replacements[replacement.getFilePath().str()].add(replacement));
}

void Annotator::AnnotateBaseCases() const {
  InferPointerVars();

  InferInitAddRef();

  InferFields();

  InferOwnerVars();

  InferAddRefReturn();

  InferReturnNew();

  InferGetters();
}

void Annotator::InferReturnNew() const {
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(returns(RefCountPointerType()),
                        hasAnyBody(hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(cxxNewExpr()))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }
}
void Annotator::InferOwnerVars()
    const {  // N.B. we don't need to use the fully qualified name
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
    AnnotateVarOwner(owner_var);
  }

  for (const auto* owner_var : NodesFromMatch<clang::VarDecl>(
           varDecl(varDecl().bind("owner_var"),
                   RefCountVarInitializedOrAssigned(
                       ignoringParenCasts(cxxNewExpr()))),
           "owner_var")) {
    AnnotateVarOwner(owner_var);
  }
}

void Annotator::InferGetters() const {
  for (const auto* f : NodesFromMatch<clang::CXXMethodDecl>(
           cxxMethodDecl(
               hasAnyBody(stmt(
                   hasDescendant(returnStmt(
                       hasReturnValue(ignoringParenImpCasts(FieldReferenceFor(
                           fieldDecl(hasType(RefCountPointerType()))
                               .bind("field")))))),
                   unless(hasDescendant(stmt(AddRefOrAssign(
                       FieldReferenceFor(equalsBoundNode("field")))))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnPointer(f);
  }
}

void Annotator::InferFields() const {
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
  // definition of the destructor. In either case, we hope it's good enough
  // for us to see a Release() if there's any.
  auto has_destructor_in_translation_unit =
      hasMethod(cxxDestructorDecl(anyOf(hasAnyBody(stmt()), isDefaulted())));
  auto has_no_destructor = unless(hasMethod(cxxDestructorDecl()));
  for (const auto* field_decl : NodesFromMatch<clang::FieldDecl>(
           fieldDecl(unless(hasDeclContext(
                         recordDecl(hasParent(classTemplateDecl())))),
                     hasType(RefCountPointerType()),
                     anyOf(hasDeclContext(cxxRecordDecl(has_no_destructor)),
                           allOf(hasDeclContext(cxxRecordDecl(
                                     has_destructor_in_translation_unit)),
                                 unless(field_is_released))))
               .bind("field"),
           "field")) {
    AnnotateFieldPointer(field_decl);
  }
}

void Annotator::InferPointerVars() const {
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(hasType(RefCountPointerType()), Unnamed(),
                       unless(isInstantiated()),
                       hasDeclContext(functionDecl(hasBody(stmt()))))
               .bind("param"),
           "param")) {
    AnnotateVarPointer(param);
  }
}

void Annotator::InferInitAddRef() const {
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           declStmt(hasSingleDecl(varDecl(hasLocalStorage()).bind("var")),
                    StmtIsImmediatelyBefore(
                        AddRefOn(declRefExpr(to(equalsBoundNode("var")))))),
           "var")) {
    AnnotateVarOwner(var);
  }
}

void Annotator::InferAddRefReturn() const {
  for (auto [v, f] : NodesFromMatch<clang::VarDecl, clang::FunctionDecl>(
           returnStmt(
               ReturnAfterAddRef(declRefExpr(to(varDecl().bind("var"))),
                                 declRefExpr(to(equalsBoundNode("var")))),
               forFunction(functionDecl().bind("f"))),
           "var", "f")) {
    AnnotateFunctionReturnOwner(f);
    if (v->hasLocalStorage() &&
        !Match(functionDecl(hasAnyBody(
                   hasDescendant(AssignTo(declRefExpr(to(equalsNode(v))))))),
               *f) &&
        !Match(
            varDecl(hasLocalStorage(),
                    hasParent(declStmt(declCountIs(1),
                                       StmtIsImmediatelyBefore(AddRefOn(
                                           declRefExpr(to(equalsNode(v)))))))),
            *v))
      AnnotateVarPointer(v);
  }

  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           returnStmt(
               ReturnAfterAddRef(FieldReferenceFor(fieldDecl().bind("field")),
                                 FieldReferenceFor(equalsBoundNode("field"))),
               forFunction(functionDecl().bind("f"))),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }
}

void Annotator::AnnotateVarOwner(const clang::VarDecl* var) const {
  AnnotateVar(var, OwnerType(), kOwnerAnnotation);
}

void Annotator::AnnotateVarPointer(const clang::VarDecl* v) const {
  AnnotateVar(v, PointerType(), kPointerAnnotation);
}

void Annotator::AnnotateVar(const clang::VarDecl* v,
                            const TypeMatcher& annotation_matcher,
                            llvm::StringRef annotation) const {
  if (const auto* p = llvm::dyn_cast<clang::ParmVarDecl>(v)) {
    AnnotateParameter(p, annotation_matcher, annotation);
  } else {
    AnnotateOneVar(v, annotation_matcher, annotation);
  }
}

void Annotator::AnnotateParameter(const clang::ParmVarDecl* p,
                                  const TypeMatcher& annotation_matcher,
                                  llvm::StringRef annotation) const {
  const auto* f =
      llvm::cast<clang::FunctionDecl>(p->getParentFunctionOrMethod());
  auto parameter_index = p->getFunctionScopeIndex();
  AnnotateFunctionParameter(f, parameter_index, annotation_matcher, annotation);

  if (Match(cxxMethodDecl(isOverride()), *f)) {
    const auto* m = llvm::cast<clang::CXXMethodDecl>(f);
    for (const auto* o : m->overridden_methods()) {
      AnnotateFunctionParameter(o, parameter_index, annotation_matcher,
                                annotation);
    }
  }
}

void Annotator::PropagateFunctionPointers() const {
  for (auto [td, fproto] :
       NodesFromMatch<clang::TypedefNameDecl, clang::FunctionProtoType>(
           stmt(anyOf(VarInitWithTypedefFunctionPointers(
                          typedefNameDecl().bind("typedef_decl"),
                          functionProtoType().bind("fproto")),
                      CallPassingFunctionToTypedefFunctionPointer(
                          typedefNameDecl().bind("typedef_decl"),
                          functionProtoType().bind("fproto")))),
           "typedef_decl", "fproto")) {
    auto rt = fproto->getReturnType();
    if (!Match(AnnotatedType(), rt)) continue;

    if (IsOwner(rt)) {
      AnnotateTypedefFunctionProtoTypeReturnOwner(td);
    } else if (IsPointer(rt)) {
      AnnotateTypedefFunctionProtoTypeReturnPointer(td);
    }
  }
}
void Annotator::AnnotateTypedefFunctionProtoTypeReturnOwner(
    const clang::TypedefNameDecl* typedef_decl) const {
  AnnotateTypedefFunctionProtoTypeReturnType(typedef_decl, OwnerType(),
                                             kOwnerAnnotation);
}

void Annotator::AnnotateTypedefFunctionProtoTypeReturnPointer(
    const clang::TypedefNameDecl* typedef_name_decl) const {
  AnnotateTypedefFunctionProtoTypeReturnType(typedef_name_decl, PointerType(),
                                             kPointerAnnotation);
}

void Annotator::AnnotateTypedefFunctionProtoTypeReturnType(
    const clang::TypedefNameDecl* typedef_decl,
    const TypeMatcher& annotation_matcher, llvm::StringRef annotation) const {
  auto underlying_type = typedef_decl->getUnderlyingType();
  if (auto t = underlying_type; !t->isFunctionPointerType() &&
                                !t->isFunctionProtoType() &&
                                !t->isMemberFunctionPointerType())
    return;
  auto underlying_loc = typedef_decl->getTypeSourceInfo()->getTypeLoc();
  auto function_proto_type_loc =
      (underlying_type->isMemberFunctionPointerType()
           ? underlying_loc.getAs<clang::MemberPointerTypeLoc>().getPointeeLoc()
       : underlying_type->isFunctionPointerType()
           ? underlying_loc.getAs<clang::PointerTypeLoc>().getPointeeLoc()
           : underlying_loc)
          .getAsAdjusted<clang::FunctionProtoTypeLoc>();
  auto return_loc = function_proto_type_loc.getReturnLoc();
  auto return_type = return_loc.getType();

  if (Match(annotation_matcher, return_type)) return;

  auto return_source_range = return_loc.getSourceRange();
  if (return_type->getPointeeType().isLocalConstQualified())
    FindConstTokenBefore(typedef_decl->getBeginLoc(), return_source_range);
  AnnotateSourceRange(return_source_range, annotation);
}

void Annotator::PropagatePointerVars() const {
  auto CounterexampleForVar = [](auto var) {
    auto ref_to_var = declRefExpr(to(var));
    return stmt(
        anyOf(PassedAsArgumentToNonPointerParam(var),
              returnStmt(hasReturnValue(ignoringParenCasts(ref_to_var))),
              InitializingOrAssigningWith(ref_to_var),
              AssignTo(ref_to_var, unless(hasType(PointerType()))),
              ReleaseCallExpr(ref_to_var)));
  };
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(
               hasLocalStorage(), hasType(RefCountPointerType()), SingleDecl(),
               unless(hasInitializer(ignoringParenImpCasts(
                   CallCcacheAccessorMethodsReturningOwner()))),
               unless(IsImmediatelyBeforeAddRef()), unless(isInstantiated()),
               unless(hasType(autoType())), decl().bind("var"),
               hasDeclContext(functionDecl(hasBody(stmt()))),
               anyOf(
                   Unused(),
                   hasDeclContext(functionDecl(unless(anyOf(
                       hasBody(hasDescendant(
                           CounterexampleForVar(equalsBoundNode("var")))),
                       cxxConstructorDecl(
                           hasAnyConstructorInitializer(withInitializer(anyOf(
                               ignoringParenCasts(
                                   declRefExpr(to(equalsBoundNode("var")))),
                               PassedAsArgumentToNonPointerParam(
                                   equalsBoundNode("var")),
                               hasDescendant(PassedAsArgumentToNonPointerParam(
                                   equalsBoundNode("var"))))))))))))),
           "var")) {
    AnnotateVarPointer(var);
  }
}

clang::ASTContext& Annotator::AstContext() const { return ast_context; }
Annotator::Annotator(const ActionOptions& action_options,
                     FileToReplacements& replacements,
                     clang::ASTContext& ast_context,
                     const clang::SourceManager& source_manager,
                     const clang::LangOptions& lang_opts)
    : action_options(action_options),
      replacements(replacements),
      ast_context(ast_context),
      source_manager(source_manager),
      lang_opts(lang_opts) {}

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
