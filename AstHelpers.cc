#include "AstHelpers.h"
#include "SwitchMatcher.h"
#include "clang/AST/IgnoreExpr.h"
#include "clang/Analysis/CFG.h"
#include "clang/Lex/Lexer.h"

// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

namespace tooling = clang::tooling;

namespace orca_tidy {

TypeMatcher AnnotationType(NamedMatcher named_matcher) {
  return qualType(hasDeclaration(typeAliasTemplateDecl(named_matcher)));
}
TypeMatcher OwnerType() { return AnnotationType(hasName("::gpos::owner")); }
TypeMatcher PointerType() { return AnnotationType(hasName("::gpos::pointer")); }
TypeMatcher LeakedType() { return AnnotationType(hasName("::gpos::leaked")); }
TypeMatcher CastType() { return AnnotationType(hasName("::gpos::cast_func")); }
TypeMatcher AnnotatedType() {
  return AnnotationType(hasAnyName("::gpos::pointer", "::gpos::owner",
                                   "::gpos::leaked", "::gpos::cast_func"));
}

TypeMatcher RefCountPointerType() {
  auto ref_count_record_decl = cxxRecordDecl(isSameOrDerivedFrom(cxxRecordDecl(
      hasMethod(cxxMethodDecl(hasName("Release"), parameterCountIs(0))))));

  auto ref_count_pointer_type =
      pointsTo(hasCanonicalType(hasDeclaration(ref_count_record_decl)));
  return ref_count_pointer_type;
}

clang::TypeLoc IgnoringElaboratedQualified(clang::TypeLoc type_loc) {
  while (type_loc.getTypeLocClass() == clang::TypeLoc::Elaborated ||
         type_loc.getTypeLocClass() == clang::TypeLoc::Qualified) {
    type_loc = type_loc.getNextTypeLoc();
  }
  return type_loc;
}

void AnnotateSourceRange(
    clang::SourceRange source_range, llvm::StringRef annotation,
    const clang::ASTContext& ast_context,
    std::map<std::string, tooling::Replacements>& replacements) {
  AnnotateSourceRange(source_range, source_range, annotation, ast_context,
                      replacements);
}

void AnnotateSourceRange(
    clang::SourceRange source_range, clang::SourceRange sub_range,
    llvm::StringRef annotation, const clang::ASTContext& ast_context,
    std::map<std::string, tooling::Replacements>& replacements) {
  const auto& source_manager = ast_context.getSourceManager();
  const auto& lang_opts = ast_context.getLangOpts();

  auto replace_range = clang::CharSourceRange::getTokenRange(source_range);
  auto type_text = clang::Lexer::getSourceText(
      clang::CharSourceRange::getTokenRange(sub_range), source_manager,
      lang_opts);

  std::string new_text = (annotation + "<" + type_text + ">").str();

  tooling::Replacement replacement(source_manager, replace_range, new_text,
                                   lang_opts);
  std::string file_path = replacement.getFilePath().str();
  CantFail(replacements[file_path].add(replacement));
}

void CantFail(llvm::Error error) noexcept {
  if (!error) [[likely]]
    return;
  llvm::errs() << llvm::toString(std::move(error)) << '\n';
  std::terminate();
}

StatementMatcher CallCcacheAccessorMethodsReturningOwner() {
  return callExpr(
      callee(cxxMethodDecl(hasAnyName("Insert", "Val", "Next"),
                           ofClass(hasName("gpos::CCacheAccessor")))));
}

DeclarationMatcher SingleDecl() {
  return unless(hasParent(declStmt(unless(declCountIs(1)))));
}

StatementMatcher AssignTo(const ExpressionMatcher& lhs) {
  return binaryOperation(hasOperatorName("="), hasLHS(lhs));
}

StatementMatcher AssignTo(const ExpressionMatcher& lhs,
                          const ExpressionMatcher& rhs) {
  return binaryOperation(hasOperatorName("="), hasLHS(lhs), hasRHS(rhs));
}

namespace {
AST_MATCHER_P(clang::QualType, IgnoringElaboratedImpl, TypeMatcher,
              type_matcher) {
  return type_matcher.matches(StripElaborated(Node), Finder, Builder);
}

// FIXME: could have returned const Expr, if not for an LLVM bug
inline clang::Expr* IgnoreCastFuncsSingleStep(clang::Expr* e) {
  auto* call = llvm::dyn_cast<clang::CallExpr>(e);
  if (!call || call->getNumArgs() != 1) return e;

  auto* callee_decl = call->getCalleeDecl();
  if (IsCastFunc(callee_decl) || IsUniversalCastFunc(callee_decl)) {
    return call->getArg(0);
  }
  return e;
}

AST_MATCHER_P(clang::QualType, IgnoringAnnotationImpl, TypeMatcher,
              type_matcher) {
  return type_matcher.matches(StripAnnotation(Node), Finder, Builder);
}

AST_MATCHER_P(clang::Stmt, HasSourceRangeImpl, clang::SourceRange,
              source_range) {
  return Node.getSourceRange() == source_range;
}

AST_MATCHER_P(clang::Stmt, StmtIsImmediatelyBeforeImpl, StatementMatcher, rhs) {
  const auto* compound_stmt =
      GetParentAs<clang::CompoundStmt>(Node, Finder->getASTContext());
  if (!compound_stmt) return false;
  const auto* node_it = llvm::find(compound_stmt->body(), &Node);
  const auto* rhs_it = std::next(node_it);
  return rhs_it != compound_stmt->body_end() &&
         rhs.matches(**rhs_it, Finder, Builder);
}

AST_MATCHER_P(clang::Stmt, StmtIsImmediatelyAfterImpl, StatementMatcher, lhs) {
  const auto* compound_stmt =
      GetParentAs<clang::CompoundStmt>(Node, Finder->getASTContext());
  if (!compound_stmt) return false;
  auto node_it = llvm::find(llvm::reverse(compound_stmt->body()), &Node);
  auto lhs_it = std::next(node_it);
  return lhs_it != compound_stmt->body_rend() &&
         lhs.matches(**lhs_it, Finder, Builder);
}

AST_MATCHER(clang::Decl, IsInGposImpl) { return IsGpos(Node.getDeclContext()); }

}  // namespace

clang::QualType StripElaborated(clang::QualType qual_type) {
  while (const auto* elaborated = qual_type->getAs<clang::ElaboratedType>()) {
    qual_type = elaborated->desugar();
  }
  return qual_type;
}

TypeMatcher IgnoringElaborated(const TypeMatcher& type_matcher) {
  return IgnoringElaboratedImpl(type_matcher);
}

bool IsCastFunc(const clang::Decl* decl) {
  if (!decl) return false;
  const auto* f = llvm::dyn_cast<clang::FunctionDecl>(decl);
  if (!f) return false;

  const auto* template_specialization_type =
      f->getDeclaredReturnType()
          .getTypePtr()
          ->getAs<clang::TemplateSpecializationType>();
  if (!template_specialization_type) return false;

  const auto* template_decl =
      template_specialization_type->getTemplateName().getAsTemplateDecl();

  const auto* ii = template_decl->getIdentifier();
  return IsGpos(template_decl->getDeclContext()) && ii &&
         ii->isStr("cast_func");
}

bool IsUniversalCastFunc(const clang::Decl* d) {
  const auto* f = llvm::dyn_cast_or_null<clang::FunctionDecl>(d);
  if (!f) return false;

  const auto* ii = f->getIdentifier();
  return IsGpos(f->getDeclContext()) && ii &&
         (ii->isStr("dyn_cast") || ii->isStr("cast"));
}

const clang::Expr* IgnoreParenCastFuncs(const clang::Expr* expr) {
  return clang::IgnoreExprNodes(
      const_cast<clang::Expr*>(expr), clang::IgnoreParensSingleStep,
      clang::IgnoreCastsSingleStep, IgnoreCastFuncsSingleStep);
}

const clang::Expr* IgnoreStdMove(const clang::Expr* e) {
  if (const auto* call = llvm::dyn_cast<clang::CallExpr>(e);
      call && call->isCallToStdMove()) {
    return call->getArg(0);
  }
  return e;
}

const clang::Expr* IgnoreCastFuncs(const clang::Expr* expr) {
  // FIXME: const_cast because of a bug. Fix this upstream in LLVM
  return clang::IgnoreExprNodes(const_cast<clang::Expr*>(expr),
                                IgnoreCastFuncsSingleStep);
}

StatementMatcher CallCDynPtrArrSubscriptOn(const ExpressionMatcher& expr) {
  return cxxOperatorCallExpr(
      hasType(RefCountPointerType()),
      callee(cxxMethodDecl(hasOverloadedOperatorName("[]"),
                           ofClass(hasName("::gpos::CDynamicPtrArray")))),
      hasArgument(0, ignoringParenCasts(Deref(expr))));
}

StatementMatcher Deref(const ExpressionMatcher& expr) {
  return unaryOperator(hasOperatorName("*"),
                       hasUnaryOperand(ignoringParenCasts(expr)));
}

StatementMatcher AddrOf(const ExpressionMatcher& expr) {
  return unaryOperator(hasOperatorName("&"), hasUnaryOperand(expr));
}

TypeMatcher RefCountPointerPointerType() {
  return pointsTo(qualType(RefCountPointerType(), unless(isConstQualified())));
}

TypeMatcher RefCountPointerReferenceType() {
  return references(
      qualType(RefCountPointerType(), unless(isConstQualified())));
}

LastUseStmts LastUseStatementsOfFunc(const clang::FunctionDecl* f) {
  clang::ASTContext* context = &f->getASTContext();
  if (!f->getBody()) [[unlikely]]
    return {};

  LastUseStmts ret;
  clang::CFG::BuildOptions build_options;
  build_options.AddInitializers = true;

  auto cfg = clang::CFG::buildCFG(f, f->getBody(), context, build_options);
  if (!cfg) return ret;

  const auto* exit_block = &cfg->getExit();
  auto exiting_blocks = MakeVector(llvm::make_filter_range(
      exit_block->preds(),
      [exit_block](const clang::CFGBlock::AdjacentBlock pred) {
        if (!pred.isReachable()) return false;
        auto reachable_succs = MakeVector(llvm::make_filter_range(
            pred->succs(), [](const clang::CFGBlock::AdjacentBlock ab) {
              return ab.isReachable();
            }));
        // a branch?
        if (reachable_succs.size() != 1) return false;
        // not the "last" block?
        if (reachable_succs.front() != exit_block) return false;
        // degenerate / empty block?
        if (pred->empty()) return false;

        return true;
      }));

  for (const auto cfg_block : exiting_blocks) {
    DeclSet seen;
    for (auto elem_ref : cfg_block->rrefs()) {
      auto kind = elem_ref->getKind();
      switch (elem_ref->getKind()) {
        case clang::CFGElement::Statement:
        case clang::CFGElement::Initializer:
          break;
        case clang::CFGElement::DeleteDtor:
          continue;
        case clang::CFGElement::ScopeBegin:
        case clang::CFGElement::ScopeEnd:
        case clang::CFGElement::NewAllocator:
        case clang::CFGElement::LifetimeEnds:
        case clang::CFGElement::LoopExit:
        case clang::CFGElement::Constructor:
        case clang::CFGElement::CXXRecordTypedCall:
        case clang::CFGElement::AutomaticObjectDtor:
        case clang::CFGElement::BaseDtor:
        case clang::CFGElement::MemberDtor:
        case clang::CFGElement::TemporaryDtor:
          elem_ref.dump();
          std::terminate();
      }

      StmtOrCXXCtorInitializer stmt_or_cxx_ctor_initializer;
      const clang::Stmt* statement;
      if (elem_ref->getKind() == clang::CFGElement::Initializer) {
        auto cfg_init = elem_ref->castAs<clang::CFGInitializer>();
        stmt_or_cxx_ctor_initializer = cfg_init.getInitializer();
        statement = cfg_init.getInitializer()->getInit();
      } else {
        auto cfg_stmt = elem_ref->castAs<clang::CFGStmt>();
        stmt_or_cxx_ctor_initializer = cfg_stmt.getStmt();
        statement = cfg_stmt.getStmt();
      }
      auto MakeVarSet = [](auto&& range) {
        return DeclSet{range.begin(), range.end()};
      };
      auto vars = MakeVarSet(llvm::map_range(
          match(stmt(forEachDescendant(
                    declRefExpr(to(varDecl(unless(IsInSet(seen))).bind("v"))))),
                *statement, f->getASTContext()),
          [](const BoundNodes& bound_nodes) {
            return bound_nodes.getNodeAs<clang::VarDecl>("v");
          }));

      if (vars.empty()) continue;
      ret.push_back({stmt_or_cxx_ctor_initializer, vars});
      seen.insert(vars.begin(), vars.end());
    }
  }
  return ret;
}

bool IsUniqRefToDeclInStmt(const clang::Expr* e, const clang::Decl* d,
                           const clang::Stmt* s) {
  return match(stmt(SelfOrHasDescendant(
                   declRefExpr(unless(equalsNode(e)), to(equalsNode(d))))),
               *s, d->getASTContext())
      .empty();
}

clang::QualType StripAnnotation(clang::QualType qual_type) {
  const auto* template_specialization =
      qual_type->getAs<clang::TemplateSpecializationType>();
  if (!template_specialization or !template_specialization->isTypeAlias())
    return qual_type;
  auto* type_alias_template = llvm::cast<clang::TypeAliasTemplateDecl>(
      template_specialization->getTemplateName().getAsTemplateDecl());
  if (match(AnnotatedType(), qual_type, type_alias_template->getASTContext())
          .empty())
    return qual_type;
  return template_specialization->getArg(0).getAsType();
}

TypeMatcher IgnoringAnnotation(const TypeMatcher& inner_matcher) {
  return IgnoringAnnotationImpl(inner_matcher);
}

clang::FunctionProtoTypeLoc ExtractFunctionProtoTypeLoc(
    const clang::TypedefNameDecl* typedef_name_decl) {
  auto underlying_type = typedef_name_decl->getUnderlyingType();
  auto type_loc = typedef_name_decl->getTypeSourceInfo()->getTypeLoc();
  if (underlying_type->isFunctionProtoType()) {
    return type_loc.getAsAdjusted<clang::FunctionProtoTypeLoc>();
  }
  auto without_parens = underlying_type.IgnoreParens();
  clang::TypeLoc pointee_loc;
  if (without_parens->isFunctionPointerType())
    pointee_loc = type_loc.getAs<clang::PointerTypeLoc>().getPointeeLoc();
  else if (without_parens->isMemberFunctionPointerType())
    pointee_loc = type_loc.getAs<clang::MemberPointerTypeLoc>().getPointeeLoc();
  else
    llvm_unreachable("typedef does not contain function type");
  return pointee_loc.getAsAdjusted<clang::FunctionProtoTypeLoc>();
}

clang::TypeLoc GetPointeeLocOfFirstTemplateArg(clang::TypeLoc type_loc) {
  return type_loc.getAsAdjusted<clang::TemplateSpecializationTypeLoc>()
      .getArgLoc(0)
      .getTypeSourceInfo()
      ->getTypeLoc()
      .getAs<clang::PointerTypeLoc>()
      .getPointeeLoc();
}

StatementMatcher HasSourceRange(clang::SourceRange source_range) {
  return HasSourceRangeImpl(source_range);
}

DeclarationMatcher RefArrayDecl() {
  return classTemplateSpecializationDecl(
      IsInGpos(), hasName("CDynamicPtrArray"),
      hasTemplateArgument(1, RefersToCleanupRelease()));
}

DeclarationMatcher MethodOfHashMap() {
  return cxxMethodDecl(ofClass(HashMapDecl()));
}
static auto HMK(const TemplateArgumentMatcher& matcher) {
  return hasTemplateArgument(4, matcher);
}
static auto HMT(const TemplateArgumentMatcher& matcher) {
  return hasTemplateArgument(5, matcher);
}
static auto HMRefK() { return HMK(RefersToCleanupRelease()); }
static auto HMNotRefK() { return unless(HMRefK()); }
static auto HMRefT() { return HMT(RefersToCleanupRelease()); }
static auto HMNotRefT() { return unless(HMRefT()); }

DeclarationMatcher HashMapRefKRefTDecl() {
  return HashMapDecl({HMRefK(), HMRefT()});
}
DeclarationMatcher HashMapConvertibleDecl() {
  return HashMapDecl({HMK(unless(RefersToCleanupDelete())),
                      HMT(unless(RefersToCleanupDelete()))});
}
DeclarationMatcher HashMapIterRefKRefTDecl() {
  return HashMapIterDecl({HMRefK(), HMRefT()});
}
DeclarationMatcher HashMapIterConvertibleDecl() {
  return HashMapIterDecl({HMK(unless(RefersToCleanupDelete())),
                          HMT(unless(RefersToCleanupDelete()))});
}
StatementMatcher CallHashMapIterMethodReturningOwner() {
  return callExpr(callee(cxxMethodDecl(
      Switch()
          .Case(hasName("Value"),
                ofClass(HashMapIterDecl(
                    {HMRefT(), HMK(unless(RefersToCleanupDelete()))})))
          .Case(hasName("Key"),
                ofClass(HashMapIterDecl(
                    {HMRefK(), HMT(unless(RefersToCleanupDelete()))}))))));
}

TemplateArgumentMatcher RefersToCleanupRelease() {
  return refersToDeclaration(functionDecl(hasName("CleanupRelease")));
}
TemplateArgumentMatcher RefersToCleanupNull() {
  return refersToDeclaration(functionDecl(hasName("CleanupNULL")));
}
TemplateArgumentMatcher RefersToCleanupDelete() {
  return refersToDeclaration(functionDecl(hasName("CleanupDelete")));
}
bool RefersToCleanupRelease(const clang::TemplateArgument& argument) {
  const clang::FunctionDecl* f;
  switch (argument.getKind()) {
    default:
      return false;
    case clang::TemplateArgument::Declaration: {
      const auto* func =
          llvm::dyn_cast<clang::FunctionDecl>(argument.getAsDecl());
      if (!func) return false;
      f = func;
      break;
    }
    case clang::TemplateArgument::Expression: {
      const auto* dre =
          llvm::dyn_cast<clang::DeclRefExpr>(argument.getAsExpr());
      if (!dre) return false;
      const auto* func = llvm::dyn_cast<clang::FunctionDecl>(dre->getDecl());
      if (!func) return false;
      f = func;
      break;
    }
  }
  const auto* ii = f->getIdentifier();
  return ii && ii->isStr("CleanupRelease");
}

StatementMatcher CallRefArraySubscript() {
  return cxxOperatorCallExpr(callee(
      cxxMethodDecl(hasOverloadedOperatorName("[]"), ofClass(RefArrayDecl()))));
}

StatementMatcher FieldReferenceFor(const DeclarationMatcher& field_matcher) {
  return memberExpr(member(field_matcher),
                    hasObjectExpression(ignoringParenImpCasts(cxxThisExpr())));
}

StatementMatcher ReleaseCallExpr(const ExpressionMatcher& reference_to_field) {
  auto release = cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("Release"), parameterCountIs(0))),
      on(reference_to_field));
  auto safe_release = callExpr(
      callee(functionDecl(hasName("SafeRelease"), parameterCountIs(1))),
      hasArgument(0, reference_to_field));
  return anyOf(release, safe_release);
}

StatementMatcher AddRefOn(const ExpressionMatcher& expr_matcher) {
  return cxxMemberCallExpr(callee(cxxMethodDecl(hasName("AddRef"))),
                           on(expr_matcher));
}

bool IsInMacro(clang::SourceRange source_range) {
  return source_range.isInvalid() || source_range.getBegin().isMacroID() ||
         source_range.getEnd().isMacroID();
}

StatementMatcher StmtIsImmediatelyBefore(const StatementMatcher& rhs) {
  return StmtIsImmediatelyBeforeImpl(rhs);
}
StatementMatcher StmtIsImmediatelyAfter(const StatementMatcher& lhs) {
  return StmtIsImmediatelyAfterImpl(lhs);
}

DeclarationMatcher AutoRefDecl() {
  return classTemplateSpecializationDecl(hasName("gpos::CAutoRef"));
}

DeclarationMatcher HashMapDecl(
    llvm::ArrayRef<ClassTemplateSpecializationMatcher> args) {
  return classTemplateSpecializationDecl(hasName("::gpos::CHashMap"),
                                         classTemplateSpecializationDecl(args));
}
bool IsHashMap(const clang::ClassTemplateSpecializationDecl* d) {
  if (!IsGpos(d->getDeclContext())) return false;

  const auto* ii = d->getIdentifier();
  return ii && ii->isStr("CHashMap");
}

DeclarationMatcher HashMapIterDecl(
    llvm::ArrayRef<ClassTemplateSpecializationMatcher> args) {
  return classTemplateSpecializationDecl(hasName("::gpos::CHashMapIter"),
                                         classTemplateSpecializationDecl(args));
}

DeclarationMatcher HashSetDecl(
    llvm::ArrayRef<ClassTemplateSpecializationMatcher> matchers) {
  return classTemplateSpecializationDecl(
      hasName("::gpos::CHashSet"), classTemplateSpecializationDecl(matchers));
}

DeclarationMatcher MethodOfHashSet() {
  return cxxMethodDecl(ofClass(HashSetDecl()));
}

static StatementMatcher ForEachArgumentWithConstQualifiedRefCountParamType(
    const DeclarationMatcher& callee_decl_matcher,
    const ExpressionMatcher& arg_matcher) {
  return cxxMemberCallExpr(
      callee(callee_decl_matcher),
      ForEachArgumentWithParamType(
          arg_matcher,
          qualType(RefCountPointerType(), pointsTo(isConstQualified()))));
}

StatementMatcher ForEachArgumentToHashMapMethodWithPointerParam(
    const ExpressionMatcher& arg_matcher) {
  return ForEachArgumentWithConstQualifiedRefCountParamType(MethodOfHashMap(),
                                                            arg_matcher);
}

StatementMatcher ForEachArgumentToHashSetMethodWithPointerParam(
    const ExpressionMatcher& arg_matcher) {
  return ForEachArgumentWithConstQualifiedRefCountParamType(MethodOfHashSet(),
                                                            arg_matcher);
}

StatementMatcher ForEachArgumentToHashSetMethodWithOwnerParam(
    const ExpressionMatcher& arg_matcher) {
  auto insert_on_ref_hash_set =
      cxxMethodDecl(ofClass(RefHashSetDecl()), hasName("Insert"));
  return cxxMemberCallExpr(callee(insert_on_ref_hash_set),
                           hasArgument(0, arg_matcher));
}

DeclarationMatcher RefHashSetDecl() {
  return HashSetDecl({hasTemplateArgument(3, RefersToCleanupRelease())});
}

DeclarationMatcher RefHashSetIterDecl() {
  return HashSetIterDecl({hasTemplateArgument(3, RefersToCleanupRelease())});
}

StatementMatcher CallHashSetIterMethodReturningOwner() {
  return callExpr(
      callee(cxxMethodDecl(ofClass(RefHashSetIterDecl()), hasName("Get"))));
}

DeclarationMatcher HashSetIterDecl(
    llvm::ArrayRef<ClassTemplateSpecializationMatcher> matchers) {
  return classTemplateSpecializationDecl(
      hasName("::gpos::CHashSetIter"),
      classTemplateSpecializationDecl(matchers));
}

DeclarationMatcher IsInGpos() { return IsInGposImpl(); }

bool IsGpos(const clang::DeclContext* decl_context) {
  if (!decl_context->isNamespace()) return false;
  const auto* ns = llvm::cast<clang::NamespaceDecl>(decl_context);
  if (!ns->getParent()->isTranslationUnit()) return false;
  const auto* ii = ns->getIdentifier();
  return ii && ii->isStr("gpos");
}

bool IsCleanupRelease(const clang::FunctionDecl* f) {
  if (!IsGpos(f->getDeclContext())) return false;
  const auto* ii = f->getIdentifier();
  return ii && ii->isStr("CleanupRelease");
}

DeclarationMatcher AddRefAppendMethod() {
  return cxxMethodDecl(hasName("::gpopt::CUtils::AddRefAppend"));
}

}  // namespace orca_tidy
