#ifndef ORCATIDY__ORCATIDY_H_
#define ORCATIDY__ORCATIDY_H_

#include <map>
#include <string>
#include "clang/Tooling/Refactoring.h"

namespace orca_tidy {

struct ActionOptions {
  bool Base = true;
  bool Propagate = true;
};

class AnnotateAction {
 public:
  explicit AnnotateAction(
      std::map<std::string, clang::tooling::Replacements>& replacements,
      ActionOptions action_options)
      : replacements_(replacements), action_options_(action_options) {}
  std::unique_ptr<clang::ASTConsumer> newASTConsumer();

 private:
  std::map<std::string, clang::tooling::Replacements>& replacements_;
  ActionOptions action_options_;
};
}  // namespace orca_tidy
#endif  // ORCATIDY__ORCATIDY_H_
