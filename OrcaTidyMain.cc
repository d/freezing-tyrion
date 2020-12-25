#include "IncludeFixer.h"
#include "OrcaTidy.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/ReplacementsYaml.h"
#include "llvm/Support/CommandLine.h"

namespace cl = llvm::cl;
namespace tooling = clang::tooling;

static cl::OptionCategory common_options("orca-annotate options");

static cl::opt<std::string> export_fixes(
    "export-fixes", cl::desc(R"(YAML file to store suggested fixes in. The
stored fixes can be applied to the input source
code with clang-apply-replacements.
)"),
    cl::value_desc("filename"), cl::cat(common_options),
    cl::sub(*cl::AllSubCommands));

static cl::SubCommand base("base");
static cl::SubCommand propagate("propagate");
static cl::SubCommand fix_include("fix-include");

int AnnotateMain(clang::tooling::RefactoringTool& tool) {
  orca_tidy::ActionOptions action_options;
  if (base) {
    action_options = {true, false};
  } else if (propagate) {
    action_options = {false, true};
  }
  orca_tidy::AnnotateAction annotate_action{tool.getReplacements(),
                                            action_options};
  return tool.run(
      clang::tooling::newFrontendActionFactory(&annotate_action).get());
}

int FixIncludeMain(tooling::RefactoringTool& tool) {
  orca_tidy::IncludeFixerActionFactory include_fixer_factory(
      tool.getReplacements());
  if (export_fixes.empty()) {
    return tool.runAndSave(&include_fixer_factory);
  } else {
    return tool.run(&include_fixer_factory);
  }
}

int main(int argc, const char* argv[]) {
  tooling::CommonOptionsParser parser(argc, argv, common_options,
                                      "A tool to annotate and rewrite ORCA");
  tooling::RefactoringTool tool(parser.getCompilations(),
                                parser.getSourcePathList());

  if (*cl::TopLevelSubCommand) {
    cl::PrintHelpMessage(false, true);
    return 0;
  }

  int exit_code = [&tool]() {
    if (fix_include) {
      return FixIncludeMain(tool);
    } else {
      return AnnotateMain(tool);
    }
  }();

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

    // Export replacements.
    tooling::TranslationUnitReplacements tur;
    const auto& file_to_replacements = tool.getReplacements();
    for (const auto& entry : file_to_replacements) {
      auto file = entry.first;
      const tooling::Replacements& unclean_replacements = entry.second;
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
