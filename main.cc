#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "clang/Frontend/TextDiagnostic.h"

namespace Rofl {
using clang::ast_matchers::anyOf;
using clang::ast_matchers::anything;
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
using clang::ast_matchers::MatchFinder;
using clang::ast_matchers::materializeTemporaryExpr;
using clang::ast_matchers::parmVarDecl;
using clang::ast_matchers::qualType;
using clang::ast_matchers::references;
using clang::ast_matchers::to;
using clang::ast_matchers::varDecl;
using clang::ast_matchers::withInitializer;

const auto kPassesTempToMemoizedRef = forEachArgumentWithParam(
    expr(hasParent(materializeTemporaryExpr())).bind("Problem"),
    parmVarDecl(equalsBoundNode("ParmVar")));
const auto kForRefField = forField(hasType(references(qualType())));
const auto kSpecialCtor =
    cxxConstructorDecl(forEachConstructorInitializer(cxxCtorInitializer(
        kForRefField,
        withInitializer(declRefExpr(to(parmVarDecl().bind("ParmVar")))))));
const auto kMatcher =
    exprWithCleanups(has(cxxConstructExpr(hasDeclaration(kSpecialCtor),
                                          kPassesTempToMemoizedRef)),
                     anyOf(expr(hasParent(varDecl().bind("Var"))), anything()))
        .bind("root");

struct Yolo : MatchFinder::MatchCallback {
  void run(const MatchFinder::MatchResult& result) override {
    auto temporary = result.Nodes.getNodeAs<clang::Expr>("Problem");
    auto root = result.Nodes.getNodeAs<clang::ExprWithCleanups>("root");

    auto var = result.Nodes.getNodeAs<clang::VarDecl>("Var");

    std::string kString = var->getName();

    const clang::SourceRange& range = temporary->getSourceRange();
    clang::TextDiagnostic TD(
        llvm::outs(), result.Context->getLangOpts(),
        &result.Context->getDiagnostics().getDiagnosticOptions());
    TD.emitDiagnostic(clang::FullSourceLoc(range.getBegin(),
                                           result.Context->getSourceManager()),
                      clang::DiagnosticsEngine::Note,
                      "Materializing a temporary here, but it will be "
                      "destroyed immediately after constructing the object \"" +
                          kString + "\"",
                      clang::CharSourceRange::getTokenRange(range), llvm::None);
  }
};

class Fomo {
  MatchFinder finder_;
  Yolo yolo_;
  std::unique_ptr<clang::tooling::FrontendActionFactory> frontendActionFactory_;

 public:
  Fomo()
      : frontendActionFactory_(
            clang::tooling::newFrontendActionFactory(&finder_)) {
    finder_.addMatcher(kMatcher, &yolo_);
  }
  clang::tooling::ToolAction* getFrontEndAction() {
    return frontendActionFactory_.get();
  }
};

Fomo Action() { return {}; }
}  // namespace Rofl

int main(int argc, const char** argv) {
  llvm::cl::OptionCategory ctros_saving_refs_category(
      "dummy tool option category");
  clang::tooling::CommonOptionsParser parser(argc, argv,
                                             ctros_saving_refs_category);
  clang::tooling::ClangTool tool(parser.getCompilations(),
                                 parser.getSourcePathList());

  tool.run(Rofl::Action().getFrontEndAction());
}
