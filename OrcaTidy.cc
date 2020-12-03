#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/ReplacementsYaml.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/YAMLTraits.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

using FileToReplacements = std::map<std::string, clang::tooling::Replacements>;

class AnnotateAction {
  class AnnotateASTConsumer : public clang::ASTConsumer {
    FileToReplacements& replacements_;

   public:
    explicit AnnotateASTConsumer(FileToReplacements& replacements)
        : replacements_(replacements) {}

    void HandleTranslationUnit(clang::ASTContext& ast_context) override {
      auto results =
          match(cxxMemberCallExpr(
                    callee(cxxMethodDecl(hasName("Release"))),
                    on(memberExpr(member(fieldDecl().bind("owner_field")),
                                  hasObjectExpression(cxxThisExpr()))),
                    hasAncestor(cxxDestructorDecl())),
                ast_context);

      clang::SourceManager& source_manager = ast_context.getSourceManager();
      const clang::LangOptions& lang_opts = ast_context.getLangOpts();

      for (auto bound_nodes : results) {
        if (auto field_decl =
                bound_nodes.getNodeAs<clang::FieldDecl>("owner_field")) {
          auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();

          auto field_type_text =
              clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(
                                              field_type_loc.getSourceRange()),
                                          source_manager, lang_opts);
          std::string new_text = ("gpos::owner<" + field_type_text + ">").str();

          clang::tooling::Replacement annotation(
              source_manager,
              clang::CharSourceRange::getTokenRange(
                  field_type_loc.getSourceRange()),
              new_text, lang_opts);
          std::string file_path = annotation.getFilePath().str();
          llvm::cantFail(replacements_[file_path].add(annotation));
        }
      }
    }
  };

 public:
  AnnotateAction(FileToReplacements& replacements)
      : replacements_(replacements) {}
  std::unique_ptr<clang::ASTConsumer> newASTConsumer() {
    return std::make_unique<AnnotateASTConsumer>(replacements_);
  }

 private:
  FileToReplacements& replacements_;
};

static llvm::cl::OptionCategory orca_tidy_category("orca-annotate options");
static llvm::cl::opt<std::string> export_fixes(
    "export-fixes", llvm::cl::desc(R"(
YAML file to store suggested fixes in. The
stored fixes can be applied to the input source
code with clang-apply-replacements.
)"),
    llvm::cl::value_desc("filename"), llvm::cl::cat(orca_tidy_category));

extern "C" int main(int argc, const char* argv[]) {
  clang::tooling::CommonOptionsParser parser(
      argc, argv, orca_tidy::orca_tidy_category,
      "A tool to annotate and rewrite ORCA");
  clang::tooling::RefactoringTool tool(parser.getCompilations(),
                                       parser.getSourcePathList());

  orca_tidy::AnnotateAction annotate_action{tool.getReplacements()};
  int exit_code;
  exit_code = tool.run(
      clang::tooling::newFrontendActionFactory(&annotate_action).get());
  if (exit_code != 0) {
    llvm::errs() << "failed to run tool.\n";
    return exit_code;
  }
  if (!export_fixes.empty()) {
    std::error_code error_code;
    llvm::raw_fd_ostream os(export_fixes, error_code, llvm::sys::fs::OF_None);
    if (error_code) {
      llvm::errs() << "Error opening output file: " << error_code.message()
                   << '\n';
      return 1;
    }

    // Export replacements_.
    clang::tooling::TranslationUnitReplacements tur;
    const auto& file_to_replacements = tool.getReplacements();
    for (const auto& entry : file_to_replacements) {
      auto file = entry.first;
      const clang::tooling::Replacements& unclean_replacements = entry.second;
      tur.Replacements.insert(tur.Replacements.end(),
                              unclean_replacements.begin(), entry.second.end());
    }

    llvm::yaml::Output yaml(os);
    yaml << tur;
    os.close();
    return 0;
  }
  return 0;
}

}  // namespace orca_tidy
