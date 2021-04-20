#include "OrcaTidy.h"
#include "AstHelpers.h"
#include "SwitchMatcher.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/FormatVariadic.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;
namespace tooling = clang::tooling;

using FileToReplacements = std::map<std::string, tooling::Replacements>;

AST_MATCHER(clang::NamedDecl, Unused) {
  return Node.hasAttr<clang::UnusedAttr>() || !Node.getDeclName() ||
         !Node.isReferenced();
}

AST_MATCHER_P(clang::CXXMethodDecl, HasOverridden, CXXMethodMatcher,
              inner_matcher) {
  for (const auto* m : Node.overridden_methods()) {
    auto result = *Builder;
    if (!inner_matcher.matches(*m, Finder, &result)) continue;
    *Builder = std::move(result);
    return true;
  }
  return false;
}

AST_MATCHER(clang::FunctionDecl, HasRedecls) {
  return Node.getFirstDecl() != Node.getMostRecentDecl();
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
  return qualType(anyOf(typedef_or_points_to_typedef,
                        arrayType(hasElementType(
                            anyOf(hasDeclaration(recordDecl(HasField(fieldDecl(
                                      hasType(typedef_or_points_to_typedef))))),
                                  typedef_or_points_to_typedef)))));
}

static ExpressionMatcher ExprInitializingFunctionPointer(
    const FunctionMatcher& function_matcher) {
  auto func = declRefExpr(to(functionDecl(function_matcher)));
  auto p_func = AddrOf(func);
  auto ref_to_func = anyOf(func, p_func);
  return ignoringParenImpCasts(
      anyOf(ref_to_func, initListExpr(forEachDescendant(expr(ref_to_func)))));
}

static StatementMatcher VarInitWithTypedefFunctionPointers(
    const DeclarationMatcher& typedef_matcher,
    const FunctionMatcher& function_matcher) {
  return declStmt(forEach(varDecl(
      hasType(TypeContainingTypedefFunctionPointer(typedef_matcher)),
      hasInitializer(ExprInitializingFunctionPointer(function_matcher)))));
}

static StatementMatcher CallPassingFunctionToTypedefFunctionPointer(
    const DeclarationMatcher& typedef_matcher,
    const FunctionMatcher& function_matcher) {
  return CallOrConstruct(ForEachArgumentWithParamType(
      ExprInitializingFunctionPointer(function_matcher),
      TypeContainingTypedefFunctionPointer(typedef_matcher)));
}

static StatementMatcher PassedAsArgumentToNonPointerOutputParam(
    const ExpressionMatcher& expr_matcher) {
  return CallOrConstruct(ForEachArgumentWithParamType(
      expr_matcher,
      pointsTo(qualType(RefCountPointerType(), unless(PointerType())))));
}

__attribute__((const)) static DeclarationMatcher MethodOfRefArray() {
  return cxxMethodDecl(ofClass(RefArrayDecl()));
}

static auto ForEachArgumentToRefArrayMethodWithOwnerParam(
    const ExpressionMatcher& arg_matcher) {
  return cxxMemberCallExpr(
      callee(MethodOfRefArray()),
      Switch()
          .Case(callee(cxxMethodDecl(hasName("Replace"))),
                hasArgument(1, IgnoringParenCastFuncs(arg_matcher)))
          .Case(callee(cxxMethodDecl(hasName("Append"))),
                hasArgument(0, IgnoringParenCastFuncs(arg_matcher))));
}

static StatementMatcher CallHashMapFindOn(const ExpressionMatcher& expr) {
  return cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("Find"), MethodOfHashMap())), on(expr));
}

// very very special case, sigh SMH
static auto ForEachArgumentToUlongToExprArrayMapWithOwnerParam(
    const ExpressionMatcher& arg_matcher) {
  return cxxMemberCallExpr(
      thisPointerType(IgnoringAnnotation(
          pointsTo(typedefNameDecl(hasName("UlongToExprArrayMap"))))),
      callee(MethodOfHashMap()),
      ForEachArgumentWithParamType(
          arg_matcher, qualType(RefCountPointerType(),
                                unless(pointsTo(isConstQualified())))));
}

static StatementMatcher ForEachArgumentToHashMapMethodWithOwnerParam(
    const ExpressionMatcher& arg_matcher) {
  auto OnHashMap = [](llvm::ArrayRef<ClassTemplateSpecializationMatcher>
                          class_template_specialization_matchers) {
    return allOf(MethodOfHashMap(),
                 ofClass(classTemplateSpecializationDecl(
                     class_template_specialization_matchers)));
  };

  auto Call = [](llvm::StringRef name, const CXXMethodMatcher& method) {
    return callee(cxxMethodDecl(hasName(name), method));
  };

  auto CallInsert = [Call](const CXXMethodMatcher& method) {
    return Call("Insert", method);
  };

  auto k_is_ref = hasTemplateArgument(4, RefersToCleanupRelease());
  auto k_is_not_ref = hasTemplateArgument(4, unless(RefersToCleanupRelease()));
  auto t_is_ref = hasTemplateArgument(5, RefersToCleanupRelease());
  auto t_is_not_ref = hasTemplateArgument(5, unless(RefersToCleanupRelease()));

  auto CallInsertOnHashMapRefKRefT =
      CallInsert(OnHashMap({k_is_ref, t_is_ref}));
  auto CallInsertOnHashMapRefK =
      CallInsert(OnHashMap({k_is_ref, t_is_not_ref}));
  auto CallInsertOnHashMapRefT =
      CallInsert(OnHashMap({k_is_not_ref, t_is_ref}));

  auto CallReplaceOnHashMapRefT = Call("Replace", OnHashMap({t_is_ref}));

  return cxxMemberCallExpr(
      Switch()
          .Case(CallInsertOnHashMapRefKRefT,
                ForEachArgumentWithParamType(arg_matcher, qualType()))
          .Case(CallInsertOnHashMapRefK,
                hasArgument(0, IgnoringParenCastFuncs(arg_matcher)))
          .Case(CallInsertOnHashMapRefT,
                hasArgument(1, IgnoringParenCastFuncs(arg_matcher)))
          .Case(CallReplaceOnHashMapRefT,
                hasArgument(1, IgnoringParenCastFuncs(arg_matcher))));
}

AST_MATCHER(clang::VarDecl, IsImmediatelyBeforeAddRef) {
  return varDecl(hasParent(declStmt(StmtIsImmediatelyBefore(
                     AddRefOn(declRefExpr(to(equalsNode(&Node))))))))
      .matches(Node, Finder, Builder);
}

static DeclarationMatcher VarFuncTypedefFuncProtoTypeReturns(
    const TypeMatcher& annotation_matcher);

class Annotator : public AstHelperMixin<Annotator> {
 public:
  Annotator(const ActionOptions& action_options,
            FileToReplacements& replacements, clang::ASTContext& ast_context,
            const clang::SourceManager& source_manager,
            const clang::LangOptions& lang_opts);
  clang::ASTContext& AstContext() const;

  void Annotate() {
    if (action_options_.Base) AnnotateBaseCases();

    if (action_options_.Propagate)
      while (Propagate())
        ;
  }

 private:
  ActionOptions action_options_;
  FileToReplacements& file_to_replaces_;
  clang::ASTContext& ast_context_;
  const clang::SourceManager& source_manager_;
  const clang::LangOptions& lang_opts_;
  mutable bool changed_ = false;
  mutable DeclSet owner_vars_, pointer_vars_;

  bool Propagate() const;

  void AnnotateBaseCases() const;

  auto FieldReleased() const {
    return IsInSet(DeclSetFromMatchAST(
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
    if (IsCleanupRelease(function)) return;

    for (const auto* f : function->redecls()) {
      const auto* parm = f->getParamDecl(parameter_index);
      AnnotateOneVar(parm, annotation_matcher, annotation);
    }
  }

  void PropagateVirtualFunctionReturnTypes() const {
    for (auto [m, rt] : NodesFromMatchAST<clang::CXXMethodDecl, clang::Type>(
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
      // You can't put auto between angle brackets:
      if (Match(qualType(anyOf(autoType(), pointsTo(autoType()))), rt))
        continue;
      if (IsAnnotated(rt)) std::terminate();

      AnnotateFunctionReturnType(f, annotation);

      if (auto kind = f->getTemplateSpecializationKind();
          isTemplateInstantiation(kind)) {
        AnnotateFunctionReturnType(f->getTemplateInstantiationPattern(),
                                   annotation_matcher, annotation);
      }
    }
  }

  void AnnotateFunctionReturnOwner(const clang::FunctionDecl* f) const {
    AnnotateFunctionReturnType(f, OwnerType(), kOwnerAnnotation);
  }

  void AnnotateFunctionReturnPointer(const clang::FunctionDecl* f) const {
    AnnotateFunctionReturnType(f, PointerType(), kPointerAnnotation);
  }

  void RememberFunc(const clang::FunctionDecl* f,
                    llvm::StringRef annotation) const;
  void AnnotateFunctionReturnType(const clang::FunctionDecl* f,
                                  llvm::StringRef annotation) const {
    RememberFunc(f, annotation);
    auto rt = f->getReturnType();
    auto rt_range = f->getReturnTypeSourceRange();
    if (IsInMacro(rt_range)) return;
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
      RememberVar(var, annotation);

      if (!Match(SingleDecl(), *var)) {
        continue;
      }
      if (IsAnnotated(v->getType())) std::terminate();

      auto source_range = v->getTypeSourceInfo()->getTypeLoc().getSourceRange();
      // We can't really edit the result of macro expansion
      if (IsInMacro(source_range)) continue;
      if (v->getType()->getPointeeType().isLocalConstQualified() &&
          !v->getType()->isTypedefNameType()) {
        FindConstTokenBefore(v->getBeginLoc(), source_range);
      }
      AnnotateSourceRange(source_range, annotation);
    }
  }

  VarMatcher OwnerVar() const;
  VarMatcher PointerVar() const;

  void RememberVar(const clang::Decl* var, llvm::StringRef annotation) const;
  void RememberTypedefFunctionProtoType(const clang::TypedefNameDecl* td,
                                        llvm::StringRef annotation) const;

  StatementMatcher CallReturningOwner() const;

  StatementMatcher PassedAsArgumentToNonPointerParam(
      const ExpressionMatcher& expr_matcher) const;

  auto ForEachArgumentWithOwnerParam(
      const ExpressionMatcher& arg_matcher) const {
    return anyOf(
        ForEachArgumentToRefArrayMethodWithOwnerParam(arg_matcher),
        ForEachArgumentToUlongToExprArrayMapWithOwnerParam(arg_matcher),
        ForEachArgumentToHashMapMethodWithOwnerParam(arg_matcher),
        ForEachArgumentToHashSetMethodWithOwnerParam(arg_matcher),
        ForEachArgumentWithParamType(arg_matcher, OwnerType()),
        ForEachArgumentWithParam(arg_matcher, OwnerVar()));
  }

  auto ForEachArgumentWithNonPointerParam(
      const ExpressionMatcher& arg_matcher) const {
    return Switch()
        .Case(hasDeclaration(MethodOfRefArray()),
              ForEachArgumentToRefArrayMethodWithOwnerParam(arg_matcher))
        .Case(hasDeclaration(MethodOfHashSet()),
              ForEachArgumentToHashSetMethodWithOwnerParam(arg_matcher))
        .Case(hasDeclaration(MethodOfHashMap()),
              anyOf(ForEachArgumentToUlongToExprArrayMapWithOwnerParam(
                        arg_matcher),
                    ForEachArgumentToHashMapMethodWithOwnerParam(arg_matcher)))
        .Case(hasDeclaration(AddRefAppendMethod()), unless(expr()))
        .Case(hasDeclaration(functionDecl()),
              ForEachArgumentWithParam(arg_matcher, unless(PointerVar())))
        // called through a pf, or pmf
        .Default(
            ForEachArgumentWithParamType(arg_matcher, unless(PointerType())));
  }

  auto ForEachArgumentWithPointerParam(
      const ExpressionMatcher& arg_matcher) const {
    return Switch()
        .Case(hasDeclaration(MethodOfHashMap()),
              ForEachArgumentToHashMapMethodWithPointerParam(arg_matcher))
        .Case(hasDeclaration(MethodOfHashSet()),
              ForEachArgumentToHashSetMethodWithPointerParam(arg_matcher))
        .Case(hasDeclaration(AddRefAppendMethod()),
              ForEachArgumentWithParam(arg_matcher, decl()))
        .Case(hasDeclaration(functionDecl()),
              ForEachArgumentWithParam(arg_matcher, PointerVar()))
        // called through a pf, or pmf
        .Default(ForEachArgumentWithParamType(arg_matcher, PointerType()));
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

  bool IsOwner(clang::QualType type) const { return Match(OwnerType(), type); }

  bool IsPointer(clang::QualType type) const {
    return Match(PointerType(), type);
  }

  bool IsCast(clang::QualType type) const { return Match(CastType(), type); }

  bool IsLeaked(const clang::VarDecl* var) const {
    return Match(LeakedType(), var->getType());
  }

  bool IsAnnotated(clang::QualType type) const {
    return Match(AnnotatedType(), type);
  }

  bool IsOwner(const clang::Type* type) const {
    return Match(elaboratedType(namesType(OwnerType())), *type);
  }

  bool IsPointer(const clang::Type* type) const {
    return Match(elaboratedType(namesType(PointerType())), *type);
  }

  bool IsOwner(const clang::Decl* d) const;
  bool IsPointer(const clang::Decl* d) const;
  bool IsAnnotated(const clang::Decl* d) const;

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
  void AnnotateTypedefFunctionProtoTypeParams(
      const clang::TypedefNameDecl* typedef_decl,
      const clang::FunctionDecl* f) const;
  void AnnotateTypedefFunctionProtoTypeReturnType(
      const clang::TypedefNameDecl* typedef_decl,
      const clang::FunctionDecl* f) const;

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
  void CovertCallsToCastFuncs() const;
  void PropagateParameterAmongRedecls() const;
  void AnnotateMultiDecls() const;
  void AnnotateMultiVar(const clang::DeclStmt* decl_stmt,
                        llvm::StringRef annotation) const;
  void AnnotateFirstVar(const clang::DeclStmt* decl_stmt,
                        llvm::StringRef annotation) const;
  void RemoveStarFromVar(const clang::VarDecl* var) const;
  void AnnotateOutputParamOwner(const clang::ParmVarDecl* param) const;
  void AnnotateOutputParamPointer(const clang::ParmVarDecl* param) const;
  void AnnotateOutputParam(const clang::ParmVarDecl* param,
                           const TypeMatcher& annotation_matcher,
                           llvm::StringRef annotation) const;
  VarMatcher PassedToCtorInitializerForOwnerFieldOf(
      const clang::FunctionDecl* f) const;
  VarMatcher PassedToOwnerCtorBaseInitializerOf(
      const clang::FunctionDecl* f) const;
  VarMatcher AddRefdIn(const clang::FunctionDecl* f) const;
  StatementMatcher InitOrAssignNonPointerVarWith(
      const ExpressionMatcher& inner_matcher) const;
  CXXMemberCallMatcher CallGetter() const;
};

bool Annotator::Propagate() const {
  changed_ = false;

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

  for (const auto* method : NodesFromMatchAST<clang::CXXMethodDecl>(
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

  for (const auto* method : NodesFromMatchAST<clang::CXXMethodDecl>(
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
  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           NonTemplateCallOrConstruct(ForEachArgumentWithParam(
               anyOf(cxxNewExpr(), CallReturningOwner()),
               parmVarDecl(hasType(RefCountPointerType())).bind("param"))),
           "param")) {
    AnnotateParameter(param, OwnerType(), kOwnerAnnotation);
  }

  PropagateTailCall();

  PropagateFunctionPointers();

  CovertCallsToCastFuncs();

  AnnotateMultiDecls();

  return changed_;
}

void Annotator::PropagateParameterAmongRedecls() const {
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
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
  for (const auto* derived_method : NodesFromMatchAST<clang::CXXMethodDecl>(
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
  for (const auto* var : NodesFromMatchAST<clang::VarDecl>(
           varDecl(RefCountVarInitializedOrAssigned(
                       IgnoringParenCastFuncs(CallReturningOwner())))
               .bind("owner_var"),
           "owner_var")) {
    AnnotateVarOwner(var);
  }

  for (const auto* f : NodesFromMatchAST<clang::CXXConstructorDecl>(
           cxxConstructorDecl(hasBody(stmt())).bind("ctor"), "ctor")) {
    for (const auto* param : NodesFromMatchNode<clang::ParmVarDecl>(
             decl(forEachDescendant(
                 parmVarDecl(anyOf(PassedToCtorInitializerForOwnerFieldOf(f),
                                   PassedToOwnerCtorBaseInitializerOf(f)),
                             // while AddRef on the field is also
                             // theoretically possible, I scanned through
                             // the ORCA code and found the few instances of
                             // AddRef on the field was for copying it again
                             // to another owner
                             unless(AddRefdIn(f)))
                     .bind("param"))),
             *f, "param")) {
      AnnotateVarOwner(param);
    }
  }
}

VarMatcher Annotator::PassedToCtorInitializerForOwnerFieldOf(
    const clang::FunctionDecl* f) const {
  return IsInSet(DeclSetFromMatchNode(
      cxxConstructorDecl(forEachConstructorInitializer(
          cxxCtorInitializer(withInitializer(IgnoringParenCastFuncs(
                                 declRefExpr(to(parmVarDecl().bind("param"))))),
                             forField(hasType(OwnerType()))))),
      *f, "param"));
}

VarMatcher Annotator::PassedToOwnerCtorBaseInitializerOf(
    const clang::FunctionDecl* f) const {
  return IsInSet(DeclSetFromMatchNode(
      cxxConstructorDecl(forEachConstructorInitializer(cxxCtorInitializer(
          isBaseInitializer(),
          withInitializer(cxxConstructExpr(ForEachArgumentWithOwnerParam(
              declRefExpr(to(parmVarDecl().bind("param"))))))))),
      *f, "param"));
}

void Annotator::PropagateReturnOwner() const {
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           functionDecl(
               unless(cxxMethodDecl(ofClass(hasName("gpos::CCacheAccessor")))),
               returns(RefCountPointerType()),
               hasAnyBody(hasDescendant(returnStmt(hasReturnValue(
                   ignoringParenImpCasts(CallReturningOwner()))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnOwner(f);
  }

  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           returnStmt(
               hasReturnValue(ignoringParenImpCasts(declRefExpr(to(varDecl(
                   hasLocalStorage(), OwnerVar(), decl().bind("var"),
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
// 1. If a never-AddRef'd local var is the argument to an owner param, the var
// is an owner, and the argument is moved (unless this is not the only
// reference to the var).
// 2. If a local var is passed to a pointer parameter, it's a pointer (unless
// it's also passed an owner parameter).
// 3. If a never AddRef-or-assigned local pointer var is passed to a parameter,
// the parameter is a pointer.
// 4. If a local owner var is passed to a parameter, the parameter is an owner,
// and the argument is moved (unless this is not the only referenced to the
// var).
void Annotator::PropagateTailCall() const {
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           functionDecl(unless(isExpansionInSystemHeader()), hasBody(stmt()))
               .bind("f"),
           "f")) {
    auto last_use_stmts = LastUseStatementsOfFunc(f);
    auto add_refd = AddRefdIn(f);
    auto assigned = IsInSet(DeclSetFromMatchNode(
        stmt(forEachDescendant(
            AssignTo(declRefExpr(to(varDecl().bind("var")))))),
        *f->getBody(), "var"));
    auto add_ref_or_assigned = anyOf(add_refd, assigned);

    for (auto [stmt_or_cxx_ctor_initializer, vars] : last_use_stmts) {
      assert(stmt_or_cxx_ctor_initializer);
      auto is_last_use = IsInSet(vars);

      const clang::Stmt* r;
      const auto* cxx_ctor_initializer =
          stmt_or_cxx_ctor_initializer
              .dyn_cast<const clang::CXXCtorInitializer*>();

      auto IsInteresting = [](const clang::CXXCtorInitializer* initializer) {
        if (initializer->isBaseInitializer()) return false;
        const auto* init = initializer->getInit();
        if (llvm::isa<clang::DeclRefExpr>(IgnoreParenCastFuncs(init)))
          return true;
        return false;
      };

      if (cxx_ctor_initializer && !IsInteresting(cxx_ctor_initializer)) {
        r = cxx_ctor_initializer->getInit();
      } else {
        r = stmt_or_cxx_ctor_initializer.dyn_cast<const clang::Stmt*>();
      }

      auto VarAndDeclRef =
          [](const clang::CXXCtorInitializer* cxx_ctor_initializer) {
            const auto* dre = llvm::cast<clang::DeclRefExpr>(
                IgnoreParenCastFuncs(cxx_ctor_initializer->getInit()));
            const auto* var = llvm::cast<clang::VarDecl>(dre->getDecl());
            return std::tuple{var, dre};
          };
      if (r) {
        for (auto [var, arg] : NodesFromMatchNode<clang::VarDecl, clang::Expr>(
                 stmt(findAll(CallOrConstruct(ForEachArgumentWithOwnerParam(
                     expr(declRefExpr(to(
                              varDecl(hasLocalStorage(), varDecl().bind("var"),
                                      is_last_use, unless(add_refd)))),
                          expr().bind("arg")))))),
                 *r, "var", "arg")) {
          if (!IsUniqRefToDeclInStmt(arg, var, r)) continue;
          AnnotateVarOwner(var);
          MoveSourceRange(arg->getSourceRange());
        }
      } else {
        if (Match(cxxCtorInitializer(
                      withInitializer(IgnoringParenCastFuncs(
                          declRefExpr(to(parmVarDecl(is_last_use))))),
                      forField(hasType(OwnerType()))),
                  *cxx_ctor_initializer)) {
          auto [var, dre] = VarAndDeclRef(cxx_ctor_initializer);
          AnnotateVarOwner(var);
          MoveSourceRange(dre->getSourceRange());
        }
      }

      auto is_used_in_non_pointer_ctor_initializers =
          PassedToCtorInitializerForOwnerFieldOf(f);

      auto passed_to_non_pointer_param = IsInSet(DeclSetFromMatchNode(
          decl(forEachDescendant(PassedAsArgumentToNonPointerParam(
              declRefExpr(to(varDecl().bind("v")))))),
          *f, "v"));

      if (r) {
        for (const auto* var : NodesFromMatchNode<clang::VarDecl>(
                 stmt(findAll(CallOrConstruct(
                     ForEachArgumentWithPointerParam(declRefExpr(to(
                         varDecl(
                             unless(isInstantiated()), hasLocalStorage(),
                             is_last_use,
                             unless(is_used_in_non_pointer_ctor_initializers),
                             unless(passed_to_non_pointer_param))
                             .bind("var"))))))),
                 *r, "var")) {
          if (Match(stmt(SelfOrHasDescendant(PassedAsArgumentToNonPointerParam(
                        declRefExpr(to(equalsNode(var)))))),
                    *r))
            continue;

          AnnotateVarPointer(var);
        }
      } else {
        if (Match(cxxCtorInitializer(
                      withInitializer(
                          IgnoringParenCastFuncs(declRefExpr(to(parmVarDecl(
                              unless(isInstantiated()), is_last_use,
                              unless(is_used_in_non_pointer_ctor_initializers),
                              unless(passed_to_non_pointer_param)))))),
                      forField(hasType(PointerType()))),
                  *cxx_ctor_initializer)) {
          auto [var, _] = VarAndDeclRef(cxx_ctor_initializer);
          AnnotateVarPointer(var);
        }
      }

      if (r) {
        for (const auto* param : NodesFromMatchNode<clang::ParmVarDecl>(
                 stmt(findAll(
                     NonTemplateCallOrConstruct(ForEachArgumentWithParam(
                         declRefExpr(to(varDecl(hasLocalStorage(), PointerVar(),
                                                is_last_use,
                                                unless(add_ref_or_assigned)))),
                         parmVarDecl(unless(isInstantiated()))
                             .bind("param"))))),
                 *r, "param")) {
          AnnotateVarPointer(param);
        }
      }

      if (r) {
        for (
            auto [param, var, arg] :
            NodesFromMatchNode<clang::ParmVarDecl, clang::VarDecl, clang::Expr>(
                stmt(findAll(CallOrConstruct(
                    ForEachArgumentWithParam(
                        declRefExpr(
                            to(varDecl(
                                   hasLocalStorage(), OwnerVar(), is_last_use,
                                   unless(
                                       is_used_in_non_pointer_ctor_initializers))
                                   .bind("var")))
                            .bind("arg"),
                        optionally(
                            parmVarDecl(unless(isInstantiated()),
                                        unless(hasDeclContext(functionDecl(
                                            hasName("SafeRelease")))))
                                .bind("param"))),
                    unless(callExpr(IsCallToStdMove()))))),
                *r, "param", "var", "arg")) {
          if (!IsUniqRefToDeclInStmt(arg, var, r)) continue;

          // Note we can't simply use unless(passed_to_non_pointer_param),
          // because the param we potentially annotate here can be non-pointer
          // (unannotated). Also note it's important we start matching from the
          // function, not the body of the function, because we want to also
          // search through constructor initializers!
          if (Match(functionDecl(hasDescendant(CallOrConstruct(
                        ForEachArgumentWithNonPointerParam(declRefExpr(
                            unless(equalsNode(arg)), to(equalsNode(var))))))),
                    *f))
            continue;

          if (param) AnnotateVarOwner(param);
          MoveSourceRange(arg->getSourceRange());
        }
      }
    }
  }
}

VarMatcher Annotator::AddRefdIn(const clang::FunctionDecl* f) const {
  return IsInSet(DeclSetFromMatchNode(
      functionDecl(hasBody(
          forEachDescendant(AddRefOn(declRefExpr(to(varDecl().bind("var"))))))),
      *f, "var"));
}

void Annotator::MoveSourceRange(clang::SourceRange source_range) const {
  auto range = clang::CharSourceRange::getTokenRange(source_range);
  auto arg_text = GetSourceText(source_range);
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
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
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
  for (const auto* owner_var : NodesFromMatchAST<clang::VarDecl>(
           ReleaseCallExpr(declRefExpr(
               to(varDecl(unless(hasType(referenceType()))).bind("owner_var")),
               unless(forFunction(hasName("SafeRelease"))))),
           "owner_var")) {
    AnnotateVarOwner(owner_var);
  }

  for (const auto* owner_var : NodesFromMatchAST<clang::VarDecl>(
           varDecl(varDecl().bind("owner_var"),
                   RefCountVarInitializedOrAssigned(
                       IgnoringParenCastFuncs(cxxNewExpr()))),
           "owner_var")) {
    AnnotateVarOwner(owner_var);
  }
}

void Annotator::InferGetters() const {
  for (const auto* f : NodesFromMatchAST<clang::CXXMethodDecl>(
           cxxMethodDecl(
               unless(isInstantiated()), returns(RefCountPointerType()),
               hasBody(anyOf(
                   stmt(hasDescendant(returnStmt(hasReturnValue(
                            ignoringParenImpCasts(FieldReferenceFor(
                                fieldDecl(hasType(RefCountPointerType()))
                                    .bind("field")))))),
                        unless(hasDescendant(AddRefOrAssign(
                            FieldReferenceFor(equalsBoundNode("field")))))),
                   stmt(hasDescendant(
                       returnStmt(hasReturnValue(ignoringParenImpCasts(
                           anyOf(CallCDynPtrArrSubscriptOn(
                                     FieldReferenceFor(fieldDecl())),
                                 CallHashMapFindOn(
                                     FieldReferenceFor(fieldDecl())))))))))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnPointer(f);
  }
}

void Annotator::InferFields() const {
  auto field_is_released = FieldReleased();
  for (const auto* field : NodesFromMatchAST<clang::FieldDecl>(
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
  for (const auto* field_decl : NodesFromMatchAST<clang::FieldDecl>(
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
  for (const auto* var : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(RefCountPointerType()), Unused(),
                   unless(isInstantiated()),
                   hasDeclContext(functionDecl(hasBody(stmt()))))
               .bind("var"),
           "var")) {
    AnnotateVarPointer(var);
  }
}

void Annotator::InferInitAddRef() const {
  for (const auto* var : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasLocalStorage(), IsImmediatelyBeforeAddRef()).bind("var"),
           "var")) {
    AnnotateVarOwner(var);
  }
}

void Annotator::InferAddRefReturn() const {
  for (auto [v, f] : NodesFromMatchAST<clang::VarDecl, clang::FunctionDecl>(
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

  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
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
  for (auto [td, f] :
       NodesFromMatchAST<clang::TypedefNameDecl, clang::FunctionDecl>(
           stmt(anyOf(VarInitWithTypedefFunctionPointers(
                          typedefNameDecl().bind("typedef_decl"),
                          functionDecl().bind("f")),
                      CallPassingFunctionToTypedefFunctionPointer(
                          typedefNameDecl().bind("typedef_decl"),
                          functionDecl().bind("f")))),
           "typedef_decl", "f")) {
    AnnotateTypedefFunctionProtoTypeReturnType(td, f);
    AnnotateTypedefFunctionProtoTypeParams(td, f);
  }
}

void Annotator::AnnotateTypedefFunctionProtoTypeReturnType(
    const clang::TypedefNameDecl* typedef_decl,
    const clang::FunctionDecl* f) const {
  if (IsAnnotated(f)) {
    if (IsOwner(f)) {
      AnnotateTypedefFunctionProtoTypeReturnOwner(typedef_decl);
    } else if (IsPointer(f)) {
      AnnotateTypedefFunctionProtoTypeReturnPointer(typedef_decl);
    }
  } else if (IsAnnotated(typedef_decl)) {
    if (IsOwner(typedef_decl))
      AnnotateFunctionReturnOwner(f);
    else if (IsPointer(typedef_decl))
      AnnotateFunctionReturnPointer(f);
  }
}

void Annotator::AnnotateTypedefFunctionProtoTypeParams(
    const clang::TypedefNameDecl* typedef_decl,
    const clang::FunctionDecl* f) const {
  auto underlying_type = typedef_decl->getUnderlyingType();
  if (auto t = underlying_type; !t->isFunctionPointerType() &&
                                !t->isFunctionProtoType() &&
                                !t->isMemberFunctionPointerType())
    return;
  auto function_type_loc = ExtractFunctionProtoTypeLoc(typedef_decl);

  for (auto [f_param, typedef_param] :
       llvm::zip(f->parameters(), function_type_loc.getParams())) {
    if (!f_param->getType()->isPointerType()) continue;
    if (IsAnnotated(f_param)) {
      // Propagate annotation from function to the typedef
      if (IsOwner(f_param))
        AnnotateVarOwner(typedef_param);
      else if (IsPointer(f_param))
        AnnotateVarPointer(typedef_param);
    } else if (IsAnnotated(typedef_param)) {
      // Propagate annotation from typedef to function
      if (IsOwner(typedef_param))
        AnnotateVarOwner(f_param);
      else if (IsPointer(typedef_param))
        AnnotateVarPointer(f_param);
    } else if (auto f_pointee = f_param->getType()->getPointeeType();
               IsAnnotated(f_pointee)) {
      if (IsOwner(f_pointee))
        AnnotateOutputParamOwner(typedef_param);
      else if (IsPointer(f_pointee))
        AnnotateOutputParamPointer(typedef_param);
    } else if (auto typedef_pointee =
                   typedef_param->getType()->getPointeeType();
               IsAnnotated(typedef_pointee)) {
      if (IsOwner(typedef_pointee))
        AnnotateOutputParamOwner(f_param);
      else if (IsPointer(typedef_pointee))
        AnnotateOutputParamPointer(f_param);
    }
  }
}

void Annotator::AnnotateOutputParamOwner(
    const clang::ParmVarDecl* param) const {
  AnnotateOutputParam(param, OwnerType(), kOwnerAnnotation);
}

void Annotator::AnnotateOutputParamPointer(
    const clang::ParmVarDecl* param) const {
  AnnotateOutputParam(param, PointerType(), kPointerAnnotation);
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

void Annotator::RememberTypedefFunctionProtoType(
    const clang::TypedefNameDecl* td, llvm::StringRef annotation) const {
  RememberVar(td, annotation);
}

void Annotator::AnnotateTypedefFunctionProtoTypeReturnType(
    const clang::TypedefNameDecl* typedef_decl,
    const TypeMatcher& annotation_matcher, llvm::StringRef annotation) const {
  auto underlying_type = typedef_decl->getUnderlyingType();
  if (auto t = underlying_type; !t->isFunctionPointerType() &&
                                !t->isFunctionProtoType() &&
                                !t->isMemberFunctionPointerType())
    return;
  auto function_type_loc = ExtractFunctionProtoTypeLoc(typedef_decl);
  auto return_loc = function_type_loc.getReturnLoc();
  auto return_type = return_loc.getType();

  if (Match(annotation_matcher, return_type)) return;

  RememberTypedefFunctionProtoType(typedef_decl, annotation);
  auto return_source_range = return_loc.getSourceRange();
  if (return_type->getPointeeType().isLocalConstQualified())
    FindConstTokenBefore(typedef_decl->getBeginLoc(), return_source_range);
  AnnotateSourceRange(return_source_range, annotation);
}

void Annotator::PropagatePointerVars() const {
  auto CounterexampleForVar = [this](auto var) {
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
  for (const auto* var : NodesFromMatchAST<clang::VarDecl>(
           varDecl(
               hasLocalStorage(), hasType(RefCountPointerType()),
               unless(IsImmediatelyBeforeAddRef()), unless(isInstantiated()),
               decl().bind("var"),
               hasDeclContext(functionDecl(hasBody(stmt()))),
               hasDeclContext(functionDecl(unless(anyOf(
                   hasBody(hasDescendant(
                       CounterexampleForVar(equalsBoundNode("var")))),
                   cxxConstructorDecl(hasAnyConstructorInitializer(
                       Switch()
                           .Case(withInitializer(IgnoringParenCastFuncs(
                                     declRefExpr(to(equalsBoundNode("var"))))),
                                 forField(hasType(OwnerType())))
                           .Default(withInitializer(SelfOrHasDescendant(
                               PassedAsArgumentToNonPointerParam(declRefExpr(
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
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           functionDecl(unless(isInstantiated()),
                        returns(qualType(RefCountPointerType(),
                                         pointsTo(isConstQualified()))))
               .bind("f"),
           "f")) {
    AnnotateFunctionReturnPointer(f);
  }

  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
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
  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
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
  for (const auto* f : NodesFromMatchAST<clang::CXXMethodDecl>(
           cxxMethodDecl(
               returns(booleanType()), IsStatic(), unless(isInstantiated()),
               hasAnyParameter(hasType(RefCountPointerType())),
               unless(hasAnyParameter(hasType(RefCountPointerPointerType()))))
               .bind("f"),
           "f")) {
    for (const auto* param : llvm::make_filter_range(
             f->parameters(), [this](const clang::ParmVarDecl* param) {
               return Match(RefCountPointerType(), param->getType());
             })) {
      AnnotateParameter(param, PointerType(), kPointerAnnotation);
    }
  }
}

void Annotator::AnnotateMultiDecls() const {
  for (const auto* decl_stmt : NodesFromMatchAST<clang::DeclStmt>(
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
    auto IsPointer = [this](auto var) { return pointer_vars_.contains(var); };
    auto IsOwner = [this](auto var) { return owner_vars_.contains(var); };

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
  auto is_output_param =
      Switch()
          .Case(hasType(RefCountPointerPointerType()), decl().bind("star_star"))
          .Case(hasType(RefCountPointerReferenceType()),
                decl().bind("star_ref"));
  auto ref_to_out_param =
      anyOf(Deref(declRefExpr(to(equalsBoundNode("star_star")))),
            declRefExpr(to(equalsBoundNode("star_ref"))));
  auto getter_added_ref = cxxMemberCallExpr(CallGetterAddedRef());
  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), is_output_param,
               decl().bind("out_param"),
               hasDeclContext(functionDecl(hasBody(stmt(hasDescendant(stmt(
                   anyOf(ReleaseCallExpr(ref_to_out_param),
                         stmt(AssignTo(IgnoringParenCastFuncs(ref_to_out_param),
                                       IgnoringParenCastFuncs(declRefExpr(
                                           to(varDecl().bind("var"))))),
                              StmtIsImmediatelyAfter(AddRefOn(
                                  declRefExpr(to(equalsBoundNode("var")))))),
                         AssignTo(
                             IgnoringParenCastFuncs(ref_to_out_param),
                             IgnoringParenCastFuncs(getter_added_ref)))))))))),
           "out_param")) {
    AnnotateOutputParamOwner(param);
  }
}

void Annotator::AnnotateOutputParam(const clang::ParmVarDecl* param,
                                    const TypeMatcher& annotation_matcher,
                                    llvm::StringRef annotation) const {
  auto scope_index = param->getFunctionScopeIndex();
  const auto* decl_context = param->getDeclContext();
  auto params_among_redecls =
      decl_context->isFunctionOrMethod()
          ? MakeVector(llvm::map_range(
                llvm::cast<clang::FunctionDecl>(decl_context)->redecls(),
                [scope_index](const clang::FunctionDecl* f) {
                  return f->getParamDecl(scope_index);
                }))
          : llvm::SmallVector{param};
  for (const auto* p : params_among_redecls) {
    auto pointer_loc = p->getTypeSourceInfo()
                           ->getTypeLoc()
                           .getAsAdjusted<clang::PointerTypeLoc>();
    auto reference_loc = p->getTypeSourceInfo()
                             ->getTypeLoc()
                             .getAsAdjusted<clang::ReferenceTypeLoc>();
    // Not spelled as a pointer nor a reference?
    if (!pointer_loc && !reference_loc) continue;

    auto pointee_loc = pointer_loc ? pointer_loc.getPointeeLoc()
                                   : reference_loc.getPointeeLoc();

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
  auto is_output_param =
      Switch()
          .Case(hasType(RefCountPointerPointerType()), decl().bind("star_star"))
          .Case(hasType(RefCountPointerReferenceType()),
                decl().bind("star_ref"));
  auto ref_to_out_param =
      anyOf(Deref(declRefExpr(to(equalsBoundNode("star_star")))),
            declRefExpr(to(equalsBoundNode("star_ref"))));
  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           parmVarDecl(unless(isInstantiated()), is_output_param,
                       decl().bind("out_param"),
                       hasDeclContext(functionDecl(hasBody(stmt(
                           hasDescendant(AssignTo(
                               ignoringParenCasts(ref_to_out_param),
                               IgnoringParenCastFuncs(anyOf(
                                   cxxNewExpr(), CallReturningOwner(),
                                   declRefExpr(to(varDecl(OwnerVar()))))))),
                           unless(hasDescendant(
                               expr(unless(ReleaseCallExpr(ref_to_out_param)),
                                    PassedAsArgumentToNonPointerParam(
                                        ref_to_out_param))))))))),
           "out_param")) {
    AnnotateOutputParamOwner(param);
  }

  auto assign_pointer_var =
      stmt(AssignTo(IgnoringParenCastFuncs(ref_to_out_param),
                    IgnoringParenCastFuncs(
                        declRefExpr(to(varDecl(PointerVar()).bind("var"))))),
           unless(StmtIsImmediatelyAfter(
               AddRefOn(declRefExpr(to(equalsBoundNode("var")))))));
  auto assign_getter =
      AssignTo(IgnoringParenCastFuncs(ref_to_out_param),
               cxxMemberCallExpr(CallGetter(), CallGetterNeverAddedRef()));
  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           parmVarDecl(unless(isInstantiated()), is_output_param,
                       decl().bind("out_param"),
                       hasDeclContext(functionDecl(hasBody(stmt(hasDescendant(
                           stmt(anyOf(assign_pointer_var, assign_getter)))))))),
           "out_param")) {
    AnnotateOutputParamPointer(param);
  }

  // The following is really code smell: the star-star really should have been a
  // single star, but oh well, we DO have those in ORCA:
  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           parmVarDecl(
               unless(isInstantiated()), is_output_param,
               decl().bind("out_param"),
               hasDeclContext(functionDecl(hasBody(unless(hasDescendant(stmt(
                   anyOf(AssignTo(IgnoringParenCastFuncs(ref_to_out_param)),
                         callExpr(ForEachArgumentWithParamType(
                             declRefExpr(to(equalsBoundNode("out_param"))),
                             RefCountPointerPointerType())))))))))),
           "out_param")) {
    AnnotateOutputParamPointer(param);
  }

  for (const auto* param : NodesFromMatchAST<clang::ParmVarDecl>(
           callExpr(ForEachArgumentWithParamType(
               declRefExpr(
                   to(parmVarDecl(unless(isInstantiated()), is_output_param)
                          .bind("out_param"))),
               pointsTo(qualType(OwnerType())))),
           "out_param")) {
    AnnotateOutputParamOwner(param);
  }

  for (auto [var, pointee_type] :
       NodesFromMatchAST<clang::VarDecl, clang::Type>(
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

void Annotator::RememberVar(const clang::Decl* var,
                            llvm::StringRef annotation) const {
  auto inserted = [var, annotation, this]() {
    if (annotation == kOwnerAnnotation) {
      return owner_vars_.insert(var).second;
    } else if (annotation == kPointerAnnotation) {
      return pointer_vars_.insert(var).second;
    }
    return false;
  }();
  if (inserted) changed_ = true;
}

VarMatcher Annotator::OwnerVar() const {
  return anyOf(hasType(OwnerType()), IsInSet(owner_vars_));
}

VarMatcher Annotator::PointerVar() const {
  return anyOf(hasType(PointerType()),
               parmVarDecl(isInstantiated(),
                           hasType(qualType(RefCountPointerType(),
                                            pointsTo(isConstQualified())))),
               IsInSet(pointer_vars_));
}

bool Annotator::IsOwner(const clang::Decl* d) const {
  return owner_vars_.contains(d) ||
         Match(VarFuncTypedefFuncProtoTypeReturns(OwnerType()), *d);
}
bool Annotator::IsPointer(const clang::Decl* d) const {
  return pointer_vars_.contains(d) ||
         Match(VarFuncTypedefFuncProtoTypeReturns(PointerType()), *d);
}
bool Annotator::IsAnnotated(const clang::Decl* d) const {
  return owner_vars_.contains(d) || pointer_vars_.contains(d) ||
         Match(VarFuncTypedefFuncProtoTypeReturns(AnnotatedType()), *d);
}

DeclarationMatcher VarFuncTypedefFuncProtoTypeReturns(
    const TypeMatcher& annotation_matcher) {
  return Switch()
      .Case(varDecl(), varDecl(hasType(annotation_matcher)))
      .Case(functionDecl(), functionDecl(returns(annotation_matcher)))
      .Default(typedefNameDecl(
          hasType(IsAnyFunctionType(Returns(annotation_matcher)))));
}

void Annotator::RememberFunc(const clang::FunctionDecl* f,
                             llvm::StringRef annotation) const {
  // Hack: just record the func in the same sets as vars
  RememberVar(f, annotation);
}

StatementMatcher Annotator::CallReturningOwner() const {
  auto returns_owner = IsInSet(owner_vars_);
  return callExpr(
      anyOf(hasType(OwnerType()), callee(functionDecl(returns_owner)),
            callee(expr(ignoringParenImpCasts(
                anyOf(hasType(typedefNameDecl(returns_owner)),
                      hasType(pointsTo(typedefNameDecl(returns_owner)))))))));
}

StatementMatcher Annotator::PassedAsArgumentToNonPointerParam(
    const ExpressionMatcher& expr_matcher) const {
  return Switch()
      .Case(CallOrConstruct(),
            CallOrConstruct(
                Switch()
                    // best effort to sniff out a callExpr in an un-instantiated
                    // function template: the callees are typically an
                    // UnresolvedLookupExpr, an UnresolvedMemberExpr, or a
                    // DependentScopeDeclRefExpr. For simplicity, just say the
                    // callee doesn't have a declaration.
                    //
                    // There's a theoretic possibility of false positive, for
                    // example calling a function that returns a function
                    // pointer, and immediately calling _that_ pointer.
                    .Case(unless(hasDeclaration(decl())),
                          hasAnyArgument(IgnoringParenCastFuncs(expr_matcher)))
                    .Default(ForEachArgumentWithNonPointerParam(expr_matcher))))
      .Default(anyOf(parenListExpr(has(expr_matcher)),
                     initListExpr(has(expr_matcher))));
}

StatementMatcher Annotator::InitOrAssignNonPointerVarWith(
    const ExpressionMatcher& inner_matcher) const {
  return anyOf(
      declStmt(
          has(varDecl(unless(PointerVar()),
                      hasInitializer(IgnoringParenCastFuncs(inner_matcher))))),
      expr(
          expr().bind("assign"),
          AssignTo(
              ignoringParenImpCasts(
                  Switch()
                      .Case(memberExpr(),
                            memberExpr(
                                member(fieldDecl(hasType(OwnerType()))
                                           .bind("field")),
                                unless(hasAncestor(expr(
                                    equalsBoundNode("assign"),
                                    anyOf(StmtIsImmediatelyBefore(AddRefOn(
                                              memberExpr(member(fieldDecl(
                                                  equalsBoundNode("field")))))),
                                          StmtIsImmediatelyAfter(
                                              AddRefOn(inner_matcher))))))))
                      .Default(unless(declRefExpr(to(varDecl(PointerVar())))))),
              IgnoringParenCastFuncs(inner_matcher))));
}

CXXMemberCallMatcher Annotator::CallGetter() const {
  return allOf(anyOf(hasType(PointerType()),
                     callee(functionDecl(IsInSet(pointer_vars_)))),
               on(declRefExpr(to(varDecl()))),
               callee(cxxMethodDecl(parameterCountIs(0))));
}

void Annotator::CovertCallsToCastFuncs() const {
  for (auto [call, enclosing_function] :
       NodesFromMatchAST<clang::CallExpr, clang::FunctionDecl>(
           callExpr(callee(functionDecl(returns(CastType()))),
                    optionally(stmt(isInTemplateInstantiation(),
                                    forFunction(functionDecl().bind("f")))))
               .bind("call"),
           "call", "f")) {
    auto source_range = call->getCallee()->getSourceRange();
    auto range = clang::CharSourceRange::getTokenRange(source_range);

    auto PointeeSpellingFromDirectCallee =
        [this](const clang::FunctionDecl* direct_callee) {
          auto pointee_source_range =
              GetPointeeLocOfFirstTemplateArg(
                  direct_callee->getFunctionTypeLoc().getReturnLoc())
                  .getSourceRange();
          return GetSourceText(pointee_source_range);
        };

    auto ExprInTemplate = [this](
                              clang::SourceRange callee_source_range,
                              const clang::FunctionDecl* enclosing_function) {
      auto* func_in_primary_template =
          enclosing_function->getTemplateInstantiationPattern();
      auto exprs_in_primary_template = NodesFromMatchNode<clang::Expr>(
          traverse(clang::TK_IgnoreUnlessSpelledInSource,
                   stmt(forEachDescendant(
                       expr(HasSourceRange(callee_source_range)).bind("e")))),
          *func_in_primary_template->getBody(), "e");
      assert(exprs_in_primary_template.size() == 1);
      return exprs_in_primary_template.front();
    };

    auto PointeeSpellingFromNestedNameSpecifier =
        [](const clang::Expr* callee_in_template) {
          assert(callee_in_template->isTypeDependent());
          const auto* dre = llvm::dyn_cast<clang::DependentScopeDeclRefExpr>(
              callee_in_template);
          const auto* me = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(
              callee_in_template);
          assert(dre || me);
          auto* callee_qualifier =
              dre ? dre->getQualifier() : me->getQualifier();
          const auto* qualifier_as_type = callee_qualifier->getAsType();
          assert(qualifier_as_type);
          const auto* template_type_parm_type =
              llvm::dyn_cast<clang::TemplateTypeParmType>(qualifier_as_type);
          assert(template_type_parm_type);
          return template_type_parm_type->getDecl()->getName();
        };

    llvm::StringRef pointee_spelling;
    if (enclosing_function) {
      const clang::Expr* callee_in_template =
          ExprInTemplate(source_range, enclosing_function);
      if (const auto* direct_callee = llvm::cast_or_null<clang::FunctionDecl>(
              callee_in_template->getReferencedDeclOfCallee())) {
        pointee_spelling = PointeeSpellingFromDirectCallee(direct_callee);
      } else {
        pointee_spelling =
            PointeeSpellingFromNestedNameSpecifier(callee_in_template);
      }
    } else {
      const auto* direct_callee = call->getDirectCallee();
      pointee_spelling = PointeeSpellingFromDirectCallee(direct_callee);
    }
    auto replacement_text =
        llvm::formatv("gpos::dyn_cast<{0}>", pointee_spelling).str();
    tooling::Replacement r{SourceManager(), range, replacement_text,
                           LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }

  std::vector<int> f(int, int);

  for (const auto* cast : NodesFromMatchAST<clang::CXXDynamicCastExpr>(
           cxxDynamicCastExpr(hasSourceExpression(
                                  IgnoringParenCastFuncs(hasType(OwnerType()))))
               .bind("cast"),
           "cast")) {
    auto dest_type_loc = cast->getTypeInfoAsWritten()
                             ->getTypeLoc()
                             .getAs<clang::PointerTypeLoc>()
                             .getPointeeLoc();
    auto pointee_spelling = GetSourceText(dest_type_loc.getSourceRange());
    auto range = clang::CharSourceRange::getTokenRange(
        cast->getOperatorLoc(), cast->getAngleBrackets().getEnd());
    tooling::Replacement r{
        SourceManager(), range,
        llvm::formatv("gpos::dyn_cast<{0}>", pointee_spelling).str(),
        LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }

  for (const auto* cast : NodesFromMatchAST<clang::CStyleCastExpr>(
           cStyleCastExpr(hasCastKind(clang::CK_BaseToDerived),
                          hasDestinationType(RefCountPointerType()))
               .bind("cast"),
           "cast")) {
    auto type_loc = cast->getTypeInfoAsWritten()->getTypeLoc();
    auto pointee_loc =
        type_loc.getAsAdjusted<clang::PointerTypeLoc>().getPointeeLoc();
    auto pointee_spelling = GetSourceText(pointee_loc.getSourceRange());

    const auto* source_expression = cast->getSubExprAsWritten();
    auto source_expression_spelling =
        GetSourceText(source_expression->getSourceRange());

    bool is_const_cast =
        source_expression->getType()->getPointeeType().isConstQualified() &&
        !pointee_loc.getType().isConstQualified();
    const char* fmtGood = "gpos::cast<{0}>({1})";
    const char* fmtNaughty = "const_cast<{0}*>(gpos::cast<{0}>({1}))";
    auto replacement_text =
        llvm::formatv(is_const_cast ? fmtNaughty : fmtGood, pointee_spelling,
                      source_expression_spelling)
            .str();

    tooling::Replacement r{
        SourceManager(),
        clang::CharSourceRange::getTokenRange(cast->getSourceRange()),
        replacement_text, LangOpts()};
    // Because of the expansive nature of this edit, it's likely to conflict
    // with other edits within the call expression. Bail and try on next
    // invocation.
    //
    // Theoretically, there still is another failure mode: we successfully
    // added the universal cast here, and go on to conflict with a subsequent
    // annotation (e.g. move) in the source expression (added in the next
    // "inner" iteration).
    llvm::consumeError(file_to_replaces_[r.getFilePath().str()].add(r));
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
