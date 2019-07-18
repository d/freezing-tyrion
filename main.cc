#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

using clang::ast_matchers::cxxConstructExpr;
using clang::ast_matchers::cxxConstructorDecl;
using clang::ast_matchers::cxxCtorInitializer;
using clang::ast_matchers::declRefExpr;
using clang::ast_matchers::equalsBoundNode;
using clang::ast_matchers::expr;
using clang::ast_matchers::exprWithCleanups;
using clang::ast_matchers::forEachArgumentWithParam;
using clang::ast_matchers::forEachConstructorInitializer;
using clang::ast_matchers::forField;
using clang::ast_matchers::has;
using clang::ast_matchers::hasDeclaration;
using clang::ast_matchers::hasParent;
using clang::ast_matchers::hasType;
using clang::ast_matchers::materializeTemporaryExpr;
using clang::ast_matchers::parmVarDecl;
using clang::ast_matchers::qualType;
using clang::ast_matchers::references;
using clang::ast_matchers::to;
using clang::ast_matchers::withInitializer;

struct Yolo : clang::ast_matchers::MatchFinder::MatchCallback {
  void run(
      const clang::ast_matchers::MatchFinder::MatchResult& Result) override {}
};
int main(int argc, const char** argv) {
  llvm::cl::OptionCategory ctros_saving_refs_category(
      "dummy tool option category");
  clang::tooling::CommonOptionsParser parser(argc, argv,
                                             ctros_saving_refs_category);
  clang::tooling::ClangTool tool(parser.getCompilations(),
                                 parser.getSourcePathList());

  const auto kPassesTempToMemoizedRef = forEachArgumentWithParam(
      expr(hasParent(materializeTemporaryExpr())).bind("Problem"),
      parmVarDecl(equalsBoundNode("ParmVar")));
  const auto kForRefField = forField(hasType(references(qualType())));
  const auto kSpecialCtor =
      cxxConstructorDecl(forEachConstructorInitializer(cxxCtorInitializer(
          kForRefField,
          withInitializer(declRefExpr(to(parmVarDecl().bind("ParmVar")))))));
  const auto kMatcher = exprWithCleanups(has(cxxConstructExpr(
      hasDeclaration(kSpecialCtor), kPassesTempToMemoizedRef)));

  clang::ast_matchers::MatchFinder finder;
  Yolo yolo;
  finder.addMatcher(kMatcher, &yolo);
  tool.run(clang::tooling::newFrontendActionFactory(&finder).get());
}
