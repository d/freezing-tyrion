#include "AstHelpers.h"
#include "clang/AST/IgnoreExpr.h"
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
TypeMatcher CastType() { return AnnotationType(hasName("::gpos::cast")); }
TypeMatcher AnnotatedType() {
  return AnnotationType(
      hasAnyName("::gpos::pointer", "::gpos::owner", "::gpos::leaked"));
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
  return binaryOperator(hasOperatorName("="), hasLHS(lhs));
}

StatementMatcher AssignTo(const ExpressionMatcher& lhs,
                          const ExpressionMatcher& rhs) {
  return binaryOperator(hasOperatorName("="), hasLHS(lhs), hasRHS(rhs));
}

namespace {
AST_MATCHER_P(clang::QualType, IgnoringElaboratedImpl, TypeMatcher,
              type_matcher) {
  return type_matcher.matches(StripElaborated(Node), Finder, Builder);
}

inline clang::Expr* IgnoreCastFuncsSingleStep(clang::Expr* e) {
  if (auto* call = llvm::dyn_cast<clang::CallExpr>(e);
      call && call->getNumArgs() > 0 && IsCastFunc(call->getCalleeDecl())) {
    return call->getArg(0);
  }
  return e;
}

AST_MATCHER_P(clang::Expr, IgnoringParenCastFuncsImpl, ExpressionMatcher,
              inner_matcher) {
  return inner_matcher.matches(*IgnoreParenCastFuncs(&Node), Finder, Builder);
}
}  // namespace

clang::QualType StripElaborated(clang::QualType qual_type) {
  while (const auto* elaborated = qual_type->getAs<clang::ElaboratedType>()) {
    qual_type = elaborated->desugar();
  }
  return qual_type;
}

TypeMatcher IgnoringElaborated(TypeMatcher type_matcher) {
  return IgnoringElaboratedImpl(type_matcher);
}

bool IsCastFunc(const clang::Decl* decl) {
  if (!decl) return false;
  return !match(functionDecl(returns(CastType())), *decl, decl->getASTContext())
              .empty();
}

ExpressionMatcher IgnoringParenCastFuncs(
    const ExpressionMatcher& inner_matcher) {
  return IgnoringParenCastFuncsImpl(inner_matcher);
}

const clang::Expr* IgnoreParenCastFuncs(const clang::Expr* expr) {
  return clang::IgnoreExprNodes(
      const_cast<clang::Expr*>(expr), clang::IgnoreParensSingleStep,
      clang::IgnoreCastsSingleStep, IgnoreCastFuncsSingleStep);
}

}  // namespace orca_tidy
