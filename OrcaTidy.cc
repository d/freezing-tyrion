#include "OrcaTidy.h"
#include "AstHelpers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/DenseSet.h"

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
  auto node_it = llvm::find(llvm::reverse(compound_stmt->body()), &Node);
  auto lhs_it = std::next(node_it);
  return lhs_it != compound_stmt->body_rend() &&
         lhs.matches(**lhs_it, Finder, Builder);
}

AST_MATCHER_P(clang::RecordDecl, HasField, DeclarationMatcher, field_matcher) {
  return matchesFirstInPointerRange(field_matcher, Node.field_begin(),
                                    Node.field_end(), Finder,
                                    Builder) != Node.field_end();
}

using GetterSet =
    llvm::DenseSet<std::tuple<const clang::Decl*, const clang::Decl*>>;

// Like cxxMemberCallExpr(on(declRefExpr(to(varDecl()))),
//                           callee(cxxMethodDecl(parameterCountIs(0))))
//
// but it also extracts the var and the method decls
__attribute__((const))
std::tuple<const clang::VarDecl*, const clang::CXXMethodDecl*>
ExtractVarAndMethodFromGetterCall(const clang::CXXMemberCallExpr* call) {
  const auto* method =
      llvm::dyn_cast_or_null<clang::CXXMethodDecl>(call->getCalleeDecl());
  if (!method || method->getNumParams() != 0) return {nullptr, nullptr};

  auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(
      call->getImplicitObjectArgument()->IgnoreParenImpCasts());
  if (!dre) return {nullptr, nullptr};
  auto* var = llvm::dyn_cast<clang::VarDecl>(dre->getDecl());
  if (!var) return {nullptr, nullptr};

  return {var, method};
}

AST_MATCHER_P(clang::CXXMemberCallExpr, IsGetterCallInSet, GetterSet,
              getter_set) {
  auto [var, method] = ExtractVarAndMethodFromGetterCall(&Node);
  // doesn't look like a getter?
  if (!var) return false;
  return getter_set.contains({var, method});
}

/// We cannot simply invert IsGetterCallInSet with an \code unless \endcode
/// or we return true for an expression that doesn't even look like a getter
AST_MATCHER_P(clang::CXXMemberCallExpr, IsGetterCallNotInSet, GetterSet,
              getter_set) {
  auto [var, method] = ExtractVarAndMethodFromGetterCall(&Node);
  // doesn't look like a getter?
  if (!var) return false;
  return !getter_set.contains({var, method});
}

__attribute__((const)) static auto CallGetter() {
  return cxxMemberCallExpr(hasType(PointerType()),
                           on(declRefExpr(to(varDecl()))),
                           callee(cxxMethodDecl(parameterCountIs(0))));
}

using ReturnMatcher = decltype(hasReturnValue(expr()));
static ReturnMatcher ReturnAfterAddRef(const ExpressionMatcher& retval,
                                       const ExpressionMatcher& addref_ref) {
  return returnStmt(hasReturnValue(IgnoringParenCastFuncs(retval)),
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
static auto CallOrConstruct(Matchers... matchers) {
  return expr(anyOf(IgnoringParenCastFuncs(callExpr(matchers...)),
                    cxxConstructExpr(matchers...)));
}

template <class... Matchers>
static auto NonTemplateCallOrConstruct(Matchers... matchers) {
  return CallOrConstruct(
      unless(hasDeclaration(functionDecl(isTemplateInstantiation()))),
      matchers...);
}

static StatementMatcher PassedAsArgumentToNonPointerOutputParam(
    const ExpressionMatcher& expr_matcher) {
  return CallOrConstruct(ForEachArgumentWithParamType(
      expr_matcher,
      pointsTo(qualType(RefCountPointerType(), unless(PointerType())))));
}

static StatementMatcher PassedAsArgumentToNonPointerParam(
    const ExpressionMatcher& expr_matcher) {
  return anyOf(
      CallOrConstruct(
          ForEachArgumentWithParamType(expr_matcher, unless(PointerType()))),
      parenListExpr(has(expr_matcher)), initListExpr(has(expr_matcher)),
      callExpr(
          callee(expr(anyOf(unresolvedLookupExpr(), unresolvedMemberExpr()))),
          hasAnyArgument(IgnoringParenCastFuncs(expr_matcher))));
}

static auto ForEachArgumentWithOwnerParam(
    const ExpressionMatcher& arg_matcher) {
  return ForEachArgumentWithParamType(arg_matcher, OwnerType());
}

static auto ForEachArgumentWithPointerParam(
    const ExpressionMatcher& arg_matcher) {
  return ForEachArgumentWithParamType(arg_matcher, PointerType());
}

static StatementMatcher InitOrAssignNonPointerVarWith(
    const ExpressionMatcher& expr) {
  return anyOf(
      declStmt(has(varDecl(unless(hasType(PointerType())),
                           hasInitializer(IgnoringParenCastFuncs(expr))))),
      AssignTo(unless(declRefExpr(to(varDecl(hasType(PointerType()))))),
               IgnoringParenCastFuncs(expr)));
}

AST_MATCHER(clang::VarDecl, IsImmediatelyBeforeAddRef) {
  return varDecl(hasParent(declStmt(StmtIsImmediatelyBefore(
                     AddRefOn(declRefExpr(to(equalsNode(&Node))))))))
      .matches(Node, Finder, Builder);
}

class Annotator : public NodesFromMatchBase<Annotator> {
 public:
  Annotator(const ActionOptions& action_options,
            FileToReplacements& replacements, clang::ASTContext& ast_context,
            const clang::SourceManager& source_manager,
            const clang::LangOptions& lang_opts);
  clang::ASTContext& AstContext() const;

  void Annotate() {
    if (action_options_.Base) AnnotateBaseCases();

    if (action_options_.Propagate) Propagate();
  }

 private:
  ActionOptions action_options_;
  FileToReplacements& file_to_replaces_;
  clang::ASTContext& ast_context_;
  const clang::SourceManager& source_manager_;
  const clang::LangOptions& lang_opts_;
  mutable llvm::StringMap<llvm::DenseSet<const clang::VarDecl*>>
      annotation_to_var_decls_;

  void Propagate() const;

  void AnnotateBaseCases() const;

  auto FieldReleased() const {
    return IsInSet(DeclSetFromMatch(
        ReleaseCallExpr(FieldReferenceFor(fieldDecl().bind("owner_field"))),
        "owner_field"));
  }

  GetterSet GettersAddedRef() const {
    auto MakeGetterSetFromMatches = [](auto matches) -> GetterSet {
      return {matches.begin(), matches.end()};
    };
    return MakeGetterSetFromMatches(llvm::map_range(
        match(AddRefOn(cxxMemberCallExpr(
                  on(declRefExpr(to(varDecl().bind("obj")))),
                  callee(cxxMethodDecl(parameterCountIs(0)).bind("method")))),
              ast_context_),
        [](const BoundNodes& bound_nodes) {
          return std::tuple{
              bound_nodes.getNodeAs<clang::VarDecl>("obj"),
              bound_nodes.getNodeAs<clang::CXXMethodDecl>("method")};
        }));
  }

  auto CallGetterAddedRef() const {
    return IsGetterCallInSet(GettersAddedRef());
  }

  auto CallGetterNeverAddedRef() const {
    return IsGetterCallNotInSet(GettersAddedRef());
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
      if (IsCast(rt)) continue;
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
    auto [file_id, offset] = source_manager_.getDecomposedLoc(begin_loc);
    auto start_of_file = source_manager_.getLocForStartOfFile(file_id);
    clang::Lexer raw_lexer(start_of_file, lang_opts_,
                           source_manager_.getCharacterData(start_of_file),
                           source_manager_.getCharacterData(begin_loc),
                           source_manager_.getCharacterData(end_loc));
    clang::Token token;
    while (!raw_lexer.LexFromRawLexer(token)) {
      if (!token.is(clang::tok::raw_identifier)) continue;
      auto& identifier_info = ast_context_.Idents.get(
          llvm::StringRef(source_manager_.getCharacterData(token.getLocation()),
                          token.getLength()));
      token.setIdentifierInfo(&identifier_info);
      token.setKind(identifier_info.getTokenID());

      if (!token.is(clang::tok::kw_const)) continue;
      if (source_manager_.isBeforeInTranslationUnit(token.getLocation(),
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
      // You can't put auto between angle brackets:
      if (Match(qualType(anyOf(autoType(), pointsTo(autoType()))),
                var->getType()))
        continue;
      if (!Match(SingleDecl(), *var)) {
        annotation_to_var_decls_[annotation].insert(var);
        continue;
      }

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
    orca_tidy::AnnotateSourceRange(source_range, annotation, ast_context_,
                                   file_to_replaces_);
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
    return !match(matcher, node, ast_context_).empty();
  }

  bool IsOwner(const clang::QualType& type) const {
    return Match(OwnerType(), type);
  }

  bool IsPointer(const clang::QualType& type) const {
    return Match(PointerType(), type);
  }

  bool IsCast(clang::QualType type) const { return Match(CastType(), type); }

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
  void InferCastFunctions() const;
  void InferPointerParamsForBoolFunctions() const;
  void PropagateReturnOwner() const;
  void InferAddRefReturn() const;
  void InferInitAddRef() const;
  void InferPointerVars() const;
  void InferFields() const;
  void InferGetters() const;
  void InferOwnerVars() const;
  void InferReturnNew() const;
  void InferConstPointers() const;
  void InferOutputParams() const;

  void PropagatePointerVars() const;
  void PropagateOwnerVars() const;
  void PropagateOutputParams() const;
  void PropagateVirtualFunctionParamTypes() const;
  void PropagateParameterAmongRedecls() const;
  void AnnotateMultiDecls() const;
  void AnnotateMultiVar(const clang::DeclStmt* decl_stmt,
                        llvm::StringRef annotation) const;
  void AnnotateFirstVar(const clang::DeclStmt* decl_stmt,
                        llvm::StringRef annotation) const;
  void RemoveStarFromVar(const clang::VarDecl* var) const;
  void AnnotateOutputParam(const clang::ParmVarDecl* param,
                           const TypeMatcher& annotation_matcher,
                           llvm::StringRef annotation) const;
};

void Annotator::Propagate() const {
  PropagatePointerVars();

  PropagateReturnOwner();

  PropagateVirtualFunctionReturnTypes();

  PropagateVirtualFunctionParamTypes();

  PropagateParameterAmongRedecls();

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

  PropagateOwnerVars();

  PropagateOutputParams();

  // It's tempting to wrap an \c ignoringParenImpCasts inside \c
  // forEachArgumentWithParam here, but note that \c forEachArgumentWithParam
  // already does that for the first argument
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           NonTemplateCallOrConstruct(ForEachArgumentWithParam(
               anyOf(cxxNewExpr(), CallReturningOwner()),
               parmVarDecl(hasType(RefCountPointerType())).bind("param"))),
           "param")) {
    AnnotateParameter(param, OwnerType(), kOwnerAnnotation);
  }

  PropagateTailCall();

  PropagateFunctionPointers();

  AnnotateMultiDecls();
}

void Annotator::PropagateParameterAmongRedecls() const {
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
}

void Annotator::PropagateVirtualFunctionParamTypes() const {
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
}

void Annotator::PropagateOwnerVars() const {
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(RefCountVarInitializedOrAssigned(
                       IgnoringParenCastFuncs(CallReturningOwner())))
               .bind("owner_var"),
           "owner_var")) {
    AnnotateVarOwner(var);
  }

  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), parmVarDecl().bind("param"),
               hasDeclContext(cxxConstructorDecl(
                   hasAnyConstructorInitializer(cxxCtorInitializer(
                       forField(hasType(OwnerType())),
                       withInitializer(IgnoringParenCastFuncs(
                           declRefExpr(to(equalsBoundNode("param"))))))),
                   // while AddRef on the field is also theoretically possible,
                   // I scanned through the ORCA code and found the few
                   // instances of AddRef on the field was for copying it again
                   // to another owner
                   hasBody(unless(hasDescendant(AddRefOn(
                       declRefExpr(to(equalsBoundNode("param")))))))))),
           "param")) {
    AnnotateVarOwner(param);
  }
}

void Annotator::PropagateReturnOwner() const {
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(returns(RefCountPointerType()),
                        hasAnyBody(hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(CallReturningOwner()))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }

  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(declRefExpr(to(varDecl(
                   hasLocalStorage(), hasType(OwnerType()), decl().bind("var"),
                   hasDeclContext(
                       functionDecl(
                           unless(isInstantiated()),
                           hasBody(unless(
                               hasDescendant(PassedAsArgumentToNonPointerParam(
                                   declRefExpr(to(equalsBoundNode("var"))))))))
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
       NodesFromMatch<clang::VarDecl, clang::Expr, clang::Stmt>(
           functionDecl(ForEachReturnOrLastStmt(
               stmt(findAll(CallOrConstruct(ForEachArgumentWithOwnerParam(expr(
                        declRefExpr(to(varDecl(
                            hasLocalStorage(), varDecl().bind("var"),
                            hasDeclContext(functionDecl(unless(
                                hasAnyBody(hasDescendant(AddRefOn(declRefExpr(
                                    to(equalsBoundNode("var")))))))))))),
                        expr().bind("arg"))))))
                   .bind("r"))),
           "var", "arg", "r")) {
    if (Match(stmt(SelfOrHasDescendant(
                  CallOrConstruct(hasAnyArgument(IgnoringParenCastFuncs(
                      expr(unless(equalsNode(arg)),
                           declRefExpr(to(equalsNode(var))))))))),
              *r))
      continue;
    AnnotateVarOwner(var);
    MoveSourceRange(arg->getSourceRange());
  }

  for (auto [var, r] : NodesFromMatch<clang::VarDecl, clang::Stmt>(
           functionDecl(ForEachReturnOrLastStmt(
               stmt(findAll(CallOrConstruct(
                        ForEachArgumentWithPointerParam(declRefExpr(to(
                            varDecl(unless(isInstantiated()), hasLocalStorage())
                                .bind("var")))))))
                   .bind("r"))),
           "var", "r")) {
    if (Match(stmt(SelfOrHasDescendant(PassedAsArgumentToNonPointerParam(
                  declRefExpr(to(equalsNode(var)))))),
              *r))
      continue;
    AnnotateVarPointer(var);
  }

  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           functionDecl(ForEachReturnOrLastStmt(
               stmt(findAll(NonTemplateCallOrConstruct(ForEachArgumentWithParam(
                   declRefExpr(to(varDecl(
                       hasLocalStorage(), hasType(PointerType()),
                       varDecl().bind("var"),
                       hasDeclContext(functionDecl(hasAnyBody(
                           stmt(unless(hasDescendant(AddRefOrAssign(declRefExpr(
                               to(equalsBoundNode("var"))))))))))))),
                   parmVarDecl(unless(isInstantiated())).bind("param"))))))),
           "param")) {
    AnnotateVarPointer(param);
  }

  for (auto [param, var, arg, r] :
       NodesFromMatch<clang::ParmVarDecl, clang::VarDecl, clang::Expr,
                      clang::Stmt>(
           functionDecl(ForEachReturnOrLastStmt(
               stmt(findAll(CallOrConstruct(
                        ForEachArgumentWithParam(
                            declRefExpr(to(varDecl(hasLocalStorage(),
                                                   hasType(OwnerType()))
                                               .bind("var")))
                                .bind("arg"),
                            optionally(parmVarDecl(unless(isInstantiated()))
                                           .bind("param"))),
                        hasDeclaration(
                            functionDecl(unless(hasName("std::move")))))))
                   .bind("r"))),
           "param", "var", "arg", "r")) {
    if (Match(stmt(SelfOrHasDescendant(
                  declRefExpr(unless(equalsNode(arg)), to(equalsNode(var))))),
              *r))
      continue;

    if (param) AnnotateVarOwner(param);
    MoveSourceRange(arg->getSourceRange());
  }
}

void Annotator::MoveSourceRange(clang::SourceRange source_range) const {
  auto range = clang::CharSourceRange::getTokenRange(source_range);
  auto arg_text =
      clang::Lexer::getSourceText(range, source_manager_, lang_opts_);
  std::string new_arg = ("std::move(" + arg_text + ")").str();
  tooling::Replacement replacement(source_manager_, range, new_arg, lang_opts_);
  llvm::cantFail(
      file_to_replaces_[replacement.getFilePath().str()].add(replacement));
}

void Annotator::AnnotateBaseCases() const {
  InferPointerVars();

  InferCastFunctions();

  InferPointerParamsForBoolFunctions();

  InferInitAddRef();

  InferFields();

  InferOwnerVars();

  InferAddRefReturn();

  InferReturnNew();

  InferConstPointers();

  InferGetters();

  InferOutputParams();

  AnnotateMultiDecls();
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
void Annotator::InferOwnerVars() const {
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
    AnnotateVarOwner(owner_var);
  }

  for (const auto* owner_var : NodesFromMatch<clang::VarDecl>(
           varDecl(varDecl().bind("owner_var"),
                   RefCountVarInitializedOrAssigned(
                       IgnoringParenCastFuncs(cxxNewExpr()))),
           "owner_var")) {
    AnnotateVarOwner(owner_var);
  }
}

void Annotator::InferGetters() const {
  for (const auto* f : NodesFromMatch<clang::CXXMethodDecl>(
           cxxMethodDecl(
               hasAnyBody(anyOf(
                   stmt(hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(FieldReferenceFor(
                                fieldDecl(hasType(RefCountPointerType()))
                                    .bind("field")))))),
                        unless(hasDescendant(AddRefOrAssign(
                            FieldReferenceFor(equalsBoundNode("field")))))),
                   stmt(hasDescendant(returnStmt(hasReturnValue(
                       ignoringParenImpCasts(CallCDynPtrArrSubscriptOn(
                           FieldReferenceFor(fieldDecl()))))))))))
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
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(hasType(RefCountPointerType()), Unused(),
                   unless(isInstantiated()),
                   hasDeclContext(functionDecl(hasBody(stmt()))))
               .bind("var"),
           "var")) {
    AnnotateVarPointer(var);
  }
}

void Annotator::InferInitAddRef() const {
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(hasLocalStorage(), IsImmediatelyBeforeAddRef()).bind("var"),
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
        !Match(varDecl(hasLocalStorage(), IsImmediatelyBeforeAddRef()), *v))
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
  const auto* decl_context = p->getDeclContext();
  if (!decl_context->isFunctionOrMethod()) {
    // probably in a function prototype
    AnnotateOneVar(p, annotation_matcher, annotation);
    return;
  }
  const auto* f = llvm::cast<clang::FunctionDecl>(decl_context);
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
        anyOf(PassedAsArgumentToNonPointerParam(ref_to_var),
              PassedAsArgumentToNonPointerOutputParam(AddrOf(ref_to_var)),
              returnStmt(hasReturnValue(IgnoringParenCastFuncs(ref_to_var))),
              InitOrAssignNonPointerVarWith(ref_to_var),
              AssignTo(ref_to_var,
                       IgnoringParenCastFuncs(unless(hasType(PointerType())))),
              ReleaseCallExpr(ref_to_var)));
  };
  for (const auto* var : NodesFromMatch<clang::VarDecl>(
           varDecl(hasLocalStorage(), hasType(RefCountPointerType()),
                   unless(hasInitializer(ignoringParenImpCasts(
                       CallCcacheAccessorMethodsReturningOwner()))),
                   unless(IsImmediatelyBeforeAddRef()),
                   unless(isInstantiated()), decl().bind("var"),
                   hasDeclContext(functionDecl(hasBody(stmt()))),
                   hasDeclContext(functionDecl(unless(anyOf(
                       hasBody(hasDescendant(
                           CounterexampleForVar(equalsBoundNode("var")))),
                       cxxConstructorDecl(
                           hasAnyConstructorInitializer(withInitializer(anyOf(
                               IgnoringParenCastFuncs(
                                   declRefExpr(to(equalsBoundNode("var")))),
                               PassedAsArgumentToNonPointerParam(
                                   declRefExpr(to(equalsBoundNode("var")))),
                               hasDescendant(PassedAsArgumentToNonPointerParam(
                                   declRefExpr(
                                       to(equalsBoundNode("var")))))))))))))),
           "var")) {
    AnnotateVarPointer(var);
  }
}

clang::ASTContext& Annotator::AstContext() const { return ast_context_; }
Annotator::Annotator(const ActionOptions& action_options,
                     FileToReplacements& replacements,
                     clang::ASTContext& ast_context,
                     const clang::SourceManager& source_manager,
                     const clang::LangOptions& lang_opts)
    : action_options_(action_options),
      file_to_replaces_(replacements),
      ast_context_(ast_context),
      source_manager_(source_manager),
      lang_opts_(lang_opts) {}

void Annotator::InferConstPointers() const {
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(unless(isInstantiated()),
                        returns(qualType(RefCountPointerType(),
                                         pointsTo(isConstQualified()))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnPointer(f);
  }

  for (const auto* v : NodesFromMatch<clang::VarDecl>(
           varDecl(unless(isInstantiated()),
                   hasType(qualType(RefCountPointerType(),
                                    pointsTo(isConstQualified()))))
               .bind("var"),
           "var")) {
    AnnotateVarPointer(v);
  }
}

void Annotator::InferCastFunctions() const {
  auto dyncast_of_param =
      ignoringParenImpCasts(cxxDynamicCastExpr(hasSourceExpression(
          ignoringParenImpCasts(declRefExpr(to(parmVarDecl()))))));
  for (const auto* f : NodesFromMatch<clang::FunctionDecl>(
           functionDecl(parameterCountIs(1),
                        returns(qualType(unless(pointsTo(isConstQualified())),
                                         RefCountPointerType())),
                        hasBody(hasDescendant(returnStmt(hasReturnValue(
                            anyOf(dyncast_of_param,
                                  ignoringParenImpCasts(declRefExpr(to(varDecl(
                                      hasInitializer(dyncast_of_param)))))))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnType(f, CastType(), kCastAnnotation);
  }
}

void Annotator::InferPointerParamsForBoolFunctions() const {
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(hasType(RefCountPointerType()), unless(isInstantiated()),
                       hasDeclContext(
                           cxxMethodDecl(returns(booleanType()), IsStatic())))
               .bind("param"),
           "param")) {
    AnnotateParameter(param, PointerType(), kPointerAnnotation);
  }
}

void Annotator::AnnotateMultiDecls() const {
  for (const auto* decl_stmt : NodesFromMatch<clang::DeclStmt>(
           declStmt(unless(declCountIs(1)),
                    containsDeclaration(
                        0, varDecl(hasType(qualType(RefCountPointerType(),
                                                    unless(AnnotatedType()))))))
               .bind("decl_stmt"),
           "decl_stmt")) {
    auto decls_as_vars =
        llvm::map_range(decl_stmt->decls(), [](const clang::Decl* decl) {
          return llvm::dyn_cast<clang::VarDecl>(decl);
        });
    auto IsInCategory = [=](llvm::StringRef annotation) {
      return [=](const clang::VarDecl* var) {
        return var && annotation_to_var_decls_[annotation].contains(var);
      };
    };
    auto IsPointer = IsInCategory(kPointerAnnotation);
    auto IsOwner = IsInCategory(kOwnerAnnotation);

    if (llvm::all_of(decls_as_vars, IsPointer))
      AnnotateMultiVar(decl_stmt, kPointerAnnotation);
    else if (llvm::all_of(decls_as_vars, IsOwner))
      AnnotateMultiVar(decl_stmt, kOwnerAnnotation);
  }
}

void Annotator::AnnotateMultiVar(const clang::DeclStmt* decl_stmt,
                                 llvm::StringRef annotation) const {
  AnnotateFirstVar(decl_stmt, annotation);
  auto vars_except_first = llvm::map_range(
      llvm::drop_begin(decl_stmt->decls()),
      [](const clang::Decl* decl) { return llvm::cast<clang::VarDecl>(decl); });
  for (const auto* var : vars_except_first) {
    RemoveStarFromVar(var);
  }
}

void Annotator::RemoveStarFromVar(const clang::VarDecl* var) const {
  auto star_loc = var->getTypeSourceInfo()
                      ->getTypeLoc()
                      .getAsAdjusted<clang::PointerTypeLoc>()
                      .getSigilLoc();
  tooling::Replacement replacement{source_manager_, star_loc, 1, ""};
  CantFail(file_to_replaces_[replacement.getFilePath().str()].add(replacement));
}

void Annotator::AnnotateFirstVar(const clang::DeclStmt* decl_stmt,
                                 llvm::StringRef annotation) const {
  auto* first_var = llvm::cast<clang::VarDecl>(*decl_stmt->decl_begin());
  auto leading_type_source_range =
      first_var->getTypeSourceInfo()->getTypeLoc().getSourceRange();
  AnnotateSourceRange(leading_type_source_range, annotation);
}

void Annotator::InferOutputParams() const {
  auto deref = Deref(declRefExpr(to(equalsBoundNode("out_param"))));
  auto getter_added_ref = cxxMemberCallExpr(CallGetterAddedRef());
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), hasType(RefCountPointerPointerType()),
               decl().bind("out_param"),
               hasDeclContext(functionDecl(hasBody(stmt(hasDescendant(stmt(
                   anyOf(ReleaseCallExpr(deref),
                         stmt(AssignTo(IgnoringParenCastFuncs(deref),
                                       IgnoringParenCastFuncs(declRefExpr(
                                           to(varDecl().bind("var"))))),
                              StmtIsImmediatelyAfter(AddRefOn(
                                  declRefExpr(to(equalsBoundNode("var")))))),
                         AssignTo(
                             IgnoringParenCastFuncs(deref),
                             IgnoringParenCastFuncs(getter_added_ref)))))))))),
           "out_param")) {
    AnnotateOutputParam(param, OwnerType(), kOwnerAnnotation);
  }
}

void Annotator::AnnotateOutputParam(const clang::ParmVarDecl* param,
                                    const TypeMatcher& annotation_matcher,
                                    llvm::StringRef annotation) const {
  auto scope_index = param->getFunctionScopeIndex();
  for (const auto* f :
       llvm::cast<clang::FunctionDecl>(param->getDeclContext())->redecls()) {
    const auto* p = f->getParamDecl(scope_index);
    auto pointer_loc = p->getTypeSourceInfo()
                           ->getTypeLoc()
                           .getAsAdjusted<clang::PointerTypeLoc>();
    // Not spelled as a pointer?
    if (!pointer_loc) continue;
    auto pointee_loc = pointer_loc.getPointeeLoc();
    if (Match(annotation_matcher, pointee_loc.getType())) continue;
    auto range = pointee_loc.getSourceRange();
    // If we're spelled as star-star, and the pointee of the pointee is const
    if (auto pointee_as_pointer_loc =
            pointee_loc.getAsAdjusted<clang::PointerTypeLoc>();
        pointee_as_pointer_loc && pointee_as_pointer_loc.getPointeeLoc()
                                      .getType()
                                      .isLocalConstQualified())
      FindConstTokenBefore(p->getBeginLoc(), range);
    AnnotateSourceRange(range, annotation);
  }
}

void Annotator::PropagateOutputParams() const {
  // Why isn't this in the base cases?
  // Not just because of the CallReturningOwner() -- we could've easily
  // duplicated that logic, but because of the part of avoiding
  // PassedAsArgumentToNonPointerParam(): we won't have meaningful annotation at
  // base rule time.
  //
  // FIXME: what if we also run this at "base" time? What will we find out?
  auto deref = Deref(declRefExpr(to(equalsBoundNode("out_param"))));
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), hasType(RefCountPointerPointerType()),
               decl().bind("out_param"),
               hasDeclContext(functionDecl(hasBody(stmt(
                   hasDescendant(AssignTo(
                       ignoringParenCasts(deref),
                       IgnoringParenCastFuncs(anyOf(
                           cxxNewExpr(), CallReturningOwner(),
                           declRefExpr(to(varDecl(hasType(OwnerType())))))))),
                   unless(hasDescendant(
                       expr(unless(ReleaseCallExpr(deref)),
                            PassedAsArgumentToNonPointerParam(deref))))))))),
           "out_param")) {
    AnnotateOutputParam(param, OwnerType(), kOwnerAnnotation);
  }

  auto assign_pointer_var =
      stmt(AssignTo(IgnoringParenCastFuncs(deref),
                    IgnoringParenCastFuncs(declRefExpr(
                        to(varDecl(hasType(PointerType())).bind("var"))))),
           unless(StmtIsImmediatelyAfter(
               AddRefOn(declRefExpr(to(equalsBoundNode("var")))))));
  auto assign_getter =
      AssignTo(IgnoringParenCastFuncs(deref),
               cxxMemberCallExpr(CallGetter(), CallGetterNeverAddedRef()));
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(unless(isInstantiated()),
                       hasType(RefCountPointerPointerType()),
                       decl().bind("out_param"),
                       hasDeclContext(functionDecl(hasBody(stmt(hasDescendant(
                           stmt(anyOf(assign_pointer_var, assign_getter)))))))),
           "out_param")) {
    AnnotateOutputParam(param, PointerType(), kPointerAnnotation);
  }

  // The following is really code smell: the star-star really should have been a
  // single star, but oh well, we DO have those in ORCA:
  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), hasType(RefCountPointerPointerType()),
               decl().bind("out_param"),
               hasDeclContext(functionDecl(hasBody(unless(hasDescendant(
                   stmt(anyOf(AssignTo(IgnoringParenCastFuncs(deref)),
                              callExpr(ForEachArgumentWithParamType(
                                  declRefExpr(to(equalsBoundNode("out_param"))),
                                  RefCountPointerPointerType())))))))))),
           "out_param")) {
    AnnotateOutputParam(param, PointerType(), kPointerAnnotation);
  }

  for (const auto* param : NodesFromMatch<clang::ParmVarDecl>(
           callExpr(ForEachArgumentWithParamType(
               declRefExpr(to(parmVarDecl(unless(isInstantiated()),
                                          hasType(RefCountPointerPointerType()))
                                  .bind("out_param"))),
               pointsTo(qualType(OwnerType())))),
           "out_param")) {
    AnnotateOutputParam(param, OwnerType(), kOwnerAnnotation);
  }

  for (auto [var, pointee_type] : NodesFromMatch<clang::VarDecl, clang::Type>(
           callExpr(ForEachArgumentWithParamType(
               AddrOf(declRefExpr(to(varDecl().bind("var")))),
               pointsTo(
                   qualType(AnnotatedType(), type().bind("pointee_type"))))),
           "var", "pointee_type")) {
    if (IsOwner(pointee_type)) {
      AnnotateVarOwner(var);
    } else if (IsPointer(pointee_type)) {
      AnnotateVarPointer(var);
    }
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
