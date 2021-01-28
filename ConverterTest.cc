#include "ConverterTest.h"
#include "Converter.h"

namespace orca_tidy {
std::string ConverterTest::runToolOverCode(std::string code) {
  std::map<std::string, clang::tooling::Replacements> file_to_replaces;
  Converter converter{file_to_replaces};

  code = kPreamble + code;

  if (!clang::tooling::runToolOnCode(
          clang::tooling::newFrontendActionFactory(&converter)->create(), code))
    std::terminate();

  if (file_to_replaces.empty()) return code;
  const auto& replacements = file_to_replaces.begin()->second;

  auto changed_code = clang::tooling::applyAllReplacements(code, replacements);
  if (!changed_code) std::terminate();

  return changed_code.get();
}
}  // namespace orca_tidy
