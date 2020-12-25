#ifndef ORCATIDY__INCLUDEFIXER_H_
#define ORCATIDY__INCLUDEFIXER_H_
#include <map>
#include <string>
#include <vector>
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Tooling.h"

namespace orca_tidy {
struct IncludeFixer : clang::PreprocessorFrontendAction {
  std::map<std::string, clang::tooling::Replacements> &file_to_replaces;

 protected:
 public:
  explicit IncludeFixer(
      std::map<std::string, clang::tooling::Replacements> &file_to_replaces);

 protected:
  bool BeginSourceFileAction(clang::CompilerInstance &CI) override;
  void ExecuteAction() override {}
};

struct IncludeFixerActionFactory : clang::tooling::FrontendActionFactory {
  std::map<std::string, clang::tooling::Replacements> &file_to_replaces;

  explicit IncludeFixerActionFactory(
      std::map<std::string, clang::tooling::Replacements> &file_to_replaces);
  std::unique_ptr<clang::FrontendAction> create() override;
};
}  // namespace orca_tidy
#endif  // ORCATIDY__INCLUDEFIXER_H_
