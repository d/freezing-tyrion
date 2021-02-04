#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "clang/Frontend/TextDiagnostic.h"

namespace Rofl {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

struct Yolo : MatchFinder::MatchCallback {
  void run(const MatchFinder::MatchResult& result) override {
    auto temporary = result.Nodes.getNodeAs<clang::Expr>("Problem");

    auto var = result.Nodes.getNodeAs<clang::VarDecl>("Var");

    std::string kString{var->getName()};

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
    const auto kPassesTempToMemoizedRef = forEachArgumentWithParam(
        expr(hasParent(materializeTemporaryExpr())).bind("Problem"),
        parmVarDecl(equalsBoundNode("ParmVar")));
    const auto kForRefField = forField(hasType(references(qualType())));
    const auto kSpecialCtor =
        cxxConstructorDecl(forEachConstructorInitializer(cxxCtorInitializer(
            kForRefField,
            withInitializer(declRefExpr(to(parmVarDecl().bind("ParmVar")))))));
    const auto kMatcher =
        exprWithCleanups(
            has(cxxConstructExpr(hasDeclaration(kSpecialCtor),
                                 kPassesTempToMemoizedRef)),
            anyOf(expr(hasParent(varDecl().bind("Var"))), anything()))
            .bind("root");
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
  auto expected_parser = clang::tooling::CommonOptionsParser::create(
      argc, argv, ctros_saving_refs_category);
  if (!expected_parser) {
    llvm::WithColor::error() << llvm::toString(expected_parser.takeError());
    return 2;
  }
  auto parser = std::move(expected_parser.get());
  clang::tooling::ClangTool tool(parser.getCompilations(),
                                 parser.getSourcePathList());

  tool.run(Rofl::Action().getFrontEndAction());
}
