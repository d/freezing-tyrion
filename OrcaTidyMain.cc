#include "OrcaTidy.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/ReplacementsYaml.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::OptionCategory orca_tidy_category("orca-annotate options");
static llvm::cl::opt<std::string> export_fixes(
    "export-fixes", llvm::cl::desc(R"(
YAML file to store suggested fixes in. The
stored fixes can be applied to the input source
code with clang-apply-replacements.
)"),
    llvm::cl::value_desc("filename"), llvm::cl::cat(orca_tidy_category));

int main(int argc, const char* argv[]) {
  clang::tooling::CommonOptionsParser parser(
      argc, argv, orca_tidy_category, "A tool to annotate and rewrite ORCA");
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
