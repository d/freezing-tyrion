#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/ReplacementsYaml.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/YAMLTraits.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

typedef std::map<std::string, clang::tooling::Replacements> FileToReplacements;
class RefPtrArrayAction {
  class callback : public MatchFinder::MatchCallback {
   private:
    FileToReplacements& replacements_;

   public:
    explicit callback(FileToReplacements& replacements)
        : replacements_(replacements) {}

    void run(const MatchFinder::MatchResult& result) override {
      if (auto t =
              result.Nodes.getNodeAs<clang::TypedefNameDecl>("typedef_decl")) {
        const auto& ref_array_typedef =
            t->getTypeSourceInfo()
                ->getTypeLoc()
                .getAs<clang::TemplateSpecializationTypeLoc>();
        const clang::TemplateArgumentLoc& element_type =
            ref_array_typedef.getArgLoc(0);

        clang::ASTContext* context = result.Context;
        clang::SourceManager* source_manager = result.SourceManager;
        const clang::LangOptions& lang_opts = context->getLangOpts();

        auto element_text =
            clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(
                                            element_type.getSourceRange()),
                                        *source_manager, lang_opts);
        std::string new_text = ("CDynamicRefArray<" + element_text + ">").str();

        clang::tooling::Replacement replacement(
            *source_manager,
            clang::CharSourceRange::getTokenRange(
                ref_array_typedef.getSourceRange()),
            new_text, lang_opts);
        llvm::cantFail(
            replacements_[replacement.getFilePath().str()].add(replacement));
      }
    }
  };

 public:
  std::unique_ptr<clang::ASTConsumer> newASTConsumer() {
    return finder_.newASTConsumer();
  }
  RefPtrArrayAction(FileToReplacements& replacements)
      : callback_(replacements) {
    auto ref_array_typedef =
        typedefNameDecl(
            hasType(qualType(hasDeclaration(classTemplateSpecializationDecl(
                hasTemplateArgument(1, refersToDeclaration(functionDecl(
                                           hasName("CleanupRelease")))),
                hasSpecializedTemplate(
                    classTemplateDecl(hasName("CDynamicPtrArray"))))))))
            .bind("typedef_decl");

    finder_.addMatcher(ref_array_typedef, &callback_);
  }

 private:
  MatchFinder finder_;
  callback callback_;
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

  orca_tidy::RefPtrArrayAction ref_ptr_array_action{tool.getReplacements()};
  int exit_code;
  exit_code = tool.run(
      clang::tooling::newFrontendActionFactory(&ref_ptr_array_action).get());
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
    clang::tooling::TranslationUnitReplacements tur;
    const auto& file_to_replacements = tool.getReplacements();
    for (const auto& Entry : file_to_replacements)
      tur.Replacements.insert(tur.Replacements.end(), Entry.second.begin(),
                              Entry.second.end());

    llvm::yaml::Output yaml(os);
    yaml << tur;
    os.close();
    return 0;
  }
  return 0;
}

}  // namespace orca_tidy
