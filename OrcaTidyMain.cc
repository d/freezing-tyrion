#include "Converter.h"
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
static cl::SubCommand convert("convert");

static cl::opt<orca_tidy::FixIncludeMode> fix_include_mode(
    "mode", cl::desc("mode of fix-include"),
    cl::values(clEnumValN(orca_tidy::kOwner, "owner", "insert \"owner.h\""),
               clEnumValN(orca_tidy::kRef, "ref",
                          "remove \"owner.h\" and insert \"Ref.h\"")),
    cl::cat(common_options), cl::sub(fix_include));

int ConvertMain(tooling::RefactoringTool& tool);
int AnnotateMain(tooling::RefactoringTool& tool) {
  orca_tidy::ActionOptions action_options;
  if (base) {
    action_options = {true, false};
  } else if (propagate) {
    action_options = {false, true};
  }
  orca_tidy::AnnotateAction annotate_action{tool.getReplacements(),
                                            action_options};
  return tool.run(tooling::newFrontendActionFactory(&annotate_action).get());
}

int FixIncludeMain(tooling::RefactoringTool& tool) {
  orca_tidy::IncludeFixerActionFactory include_fixer_factory(
      tool.getReplacements(), fix_include_mode);
  if (export_fixes.empty()) {
    return tool.runAndSave(&include_fixer_factory);
  } else {
    return tool.run(&include_fixer_factory);
  }
}

int main(int argc, const char* argv[]) {
  auto expected_parser = tooling::CommonOptionsParser::create(
      argc, argv, common_options, cl::OneOrMore,
      "A tool to annotate and rewrite ORCA");
  if (!expected_parser) {
    llvm::WithColor::error() << llvm::toString(expected_parser.takeError());
    return 2;
  }
  auto parser = std::move(expected_parser.get());
  tooling::RefactoringTool tool(parser.getCompilations(),
                                parser.getSourcePathList());

  if (*cl::TopLevelSubCommand) {
    cl::PrintHelpMessage(false, true);
    return 0;
  }

  int exit_code = [&tool]() {
    if (fix_include) {
      return FixIncludeMain(tool);
    } else if (base || propagate) {
      return AnnotateMain(tool);
    } else {
      return ConvertMain(tool);
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
    for (const auto& [file, unclean_replacements] : file_to_replacements) {
      tur.Replacements.insert(tur.Replacements.end(),
                              unclean_replacements.begin(),
                              unclean_replacements.end());
    }

    llvm::yaml::Output yaml(os);
    yaml << tur;
    os.close();
    return 0;
  }

  return 0;
}

int ConvertMain(tooling::RefactoringTool& tool) {
  orca_tidy::Converter converter{tool.getReplacements()};
  return tool.run(tooling::newFrontendActionFactory(&converter).get());
}
