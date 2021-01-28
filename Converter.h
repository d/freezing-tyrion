#ifndef ORCATIDY__CONVERTER_H_
#define ORCATIDY__CONVERTER_H_
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Core/Replacement.h"

namespace orca_tidy {
class Converter {
 public:
  explicit Converter(
      std::map<std::string, clang::tooling::Replacements>& file_to_replaces);
  std::unique_ptr<clang::ASTConsumer> newASTConsumer();

 private:
  std::map<std::string, clang::tooling::Replacements>& file_to_replaces_;
};
}  // namespace orca_tidy

#endif  // ORCATIDY__CONVERTER_H_
