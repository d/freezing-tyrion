#include "AstHelpers.h"
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
TypeMatcher CastType() { return AnnotationType(hasName("::gpos::cast")); }
TypeMatcher AnnotatedType() {
  return AnnotationType(hasAnyName("::gpos::pointer", "::gpos::owner",
                                   "::gpos::leaked", "::gpos::cast"));
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

StatementMatcher IsLastStmtOfFunc(const clang::FunctionDecl* f,
                                  clang::ASTContext* context);

using StmtSet = llvm::DenseSet<const clang::Stmt*>;

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

AST_MATCHER_P(clang::Stmt, IsInSet, StmtSet, stmts) {
  return stmts.contains(&Node);
}

AST_MATCHER_P(clang::FunctionDecl, ForEachLastStmtImpl, StatementMatcher,
              inner_matcher) {
  if (!Node.getBody()) return false;

  FunctionMatcher func_matcher = hasBody(forEachDescendant(
      stmt(IsLastStmtOfFunc(&Node, &Node.getASTContext()), inner_matcher)));
  return func_matcher.matches(Node, Finder, Builder);
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

const clang::Expr* IgnoreParenCastFuncs(const clang::Expr* expr) {
  return clang::IgnoreExprNodes(
      const_cast<clang::Expr*>(expr), clang::IgnoreParensSingleStep,
      clang::IgnoreCastsSingleStep, IgnoreCastFuncsSingleStep);
}

const clang::Expr* IgnoreCastFuncs(const clang::Expr* expr) {
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
  return pointsTo(RefCountPointerType());
}

StatementMatcher IsLastStmtOfFunc(const clang::FunctionDecl* f,
                                  clang::ASTContext* context) {
  clang::CFG::BuildOptions build_options;
  auto cfg = clang::CFG::buildCFG(f, f->getBody(), context, build_options);
  const auto* exit_block = &cfg->getExit();
  auto last_statements = llvm::map_range(
      llvm::make_filter_range(
          cfg->const_nodes(),
          [exit_block](const clang::CFGBlock* cfg_block) {
            auto reachable_succs = MakeVector(llvm::make_filter_range(
                cfg_block->succs(),
                [](const clang::CFGBlock::AdjacentBlock ab) {
                  return ab.isReachable();
                }));
            // a branch?
            if (reachable_succs.size() != 1) return false;
            // not the "last" block?
            if (reachable_succs.front() != exit_block) return false;
            // last element isn't a statement?
            if (cfg_block->empty() ||
                !cfg_block->back().getAs<clang::CFGStmt>())
              return false;
            return true;
          }),
      [](const clang::CFGBlock* cfg_block) {
        return cfg_block->back().castAs<clang::CFGStmt>().getStmt();
      });
  return IsInSet(StmtSet{last_statements.begin(), last_statements.end()});
}

FunctionMatcher ForEachReturnOrLastStmt(const StatementMatcher& inner_matcher) {
  return allOf(unless(cxxConstructorDecl()),
               ForEachLastStmtImpl(inner_matcher));
}

}  // namespace orca_tidy
