#ifndef ORCATIDY__ORCATIDY_H_
#define ORCATIDY__ORCATIDY_H_

#include <map>
#include <string>
#include "clang/Tooling/Refactoring.h"

namespace orca_tidy {
class AnnotateAction {
 public:
  explicit AnnotateAction(
      std::map<std::string, clang::tooling::Replacements>& replacements)
      : replacements_(replacements) {}
  std::unique_ptr<clang::ASTConsumer> newASTConsumer();

 private:
  std::map<std::string, clang::tooling::Replacements>& replacements_;
};
}  // namespace orca_tidy
#endif  // ORCATIDY__ORCATIDY_H_
