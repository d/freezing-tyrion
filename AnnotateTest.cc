#include "AnnotateTest.h"

namespace orca_tidy {
std::string AnnotateTest::runToolOverCode(std::string code) {
  std::map<std::string, clang::tooling::Replacements> file_to_replaces;

  orca_tidy::AnnotateAction action(file_to_replaces, action_options_);
  auto ast_consumer = action.newASTConsumer();

  const auto* kToolName = "annotate";
  const char* kRefCountHContent = R"C++(
#ifndef GPOS_CREFCOUNT_H
#define GPOS_CREFCOUNT_H
    namespace gpos {
    template <class Derived>
    struct CRefCount {
      void Release();
      void AddRef();
    };

    template <class T>
    void SafeRelease(CRefCount<T> *);
    }  // namespace gpos
#endif
  )C++";

  const char* kOwnerHContent = R"C++(
#ifndef GPOS_OWNER_H
#define GPOS_OWNER_H
#include <utility>  // for move
    namespace gpos {
    template <class T>
    using owner = T;

    template <class T>
    using pointer = T;

    template <class T>
    using leaked = T;

    template <class T>
    using cast = T;
    }  // namespace gpos
#endif
  )C++";

  code = kPreamble + code;

  std::unique_ptr<clang::ASTUnit> ast_unit =
      clang::tooling::buildASTFromCodeWithArgs(
          code, {"-std=c++14"}, kSourceFilePath, kToolName,
          std::make_shared<clang::PCHContainerOperations>(),
          clang::tooling::getClangStripDependencyFileAdjuster(),
          {{"/tmp/CRefCount.h", kRefCountHContent},
           {"/tmp/owner.h", kOwnerHContent}});

  ast_consumer->HandleTranslationUnit(ast_unit->getASTContext());

  if (file_to_replaces.empty()) return code;
  const auto& replacements = file_to_replaces.begin()->second;

  auto changed_code = clang::tooling::applyAllReplacements(code, replacements);
  if (!changed_code) std::terminate();

  return changed_code.get();
}

}  // namespace orca_tidy
