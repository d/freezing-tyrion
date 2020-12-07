#include <iterator>
#include "OrcaTidy.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"

#include "gmock/gmock-matchers.h"
#include "gtest/gtest.h"

using ::testing::SizeIs;
using ::testing::StrEq;

class OrcaTidyTest : public ::testing::Test {
 protected:
  clang::tooling::CommonOptionsParser parser_;
  clang::tooling::RefactoringTool tool_;
  constexpr static const char* const kSourceFilePath = "/tmp/foo.cc";

  auto& GetFileToReplaces() { return tool_.getReplacements(); }

  void TestBeforeAfter(const std::string& code,
                       const std::string& expected_changed_code) {
    tool_.mapVirtualFile(kSourceFilePath, code);

    auto& file_to_replaces = GetFileToReplaces();
    orca_tidy::AnnotateAction action(file_to_replaces);

    int exit_code =
        tool_.run(clang::tooling::newFrontendActionFactory(&action).get());

    ASSERT_EQ(0, exit_code);

    ASSERT_THAT(file_to_replaces, SizeIs(1));
    const auto& replacements = file_to_replaces.begin()->second;

    auto changed_code =
        clang::tooling::applyAllReplacements(code, replacements);

    if (!changed_code) FAIL() << "failed applying replacements";

    // TODO: nicer matcher that formats before comparison
    ASSERT_THAT(expected_changed_code, StrEq(*changed_code));
  }

 public:
  OrcaTidyTest()
      : parser_([] {
          llvm::cl::OptionCategory category("my-tool_ options");
          const char* argv[] = {"annotate", kSourceFilePath, "--", "-xc++"};
          int argc = std::size(argv);

          return clang::tooling::CommonOptionsParser{argc, argv, category};
        }()),
        tool_(parser_.getCompilations(), parser_.getSourcePathList()) {
    tool_.mapVirtualFile("/tmp/owner.h", R"C++(
#ifndef GPOS_OWNER_H
#define GPOS_OWNER_H
      namespace gpos {
      template <class T>
      using owner = T;
      }
#endif
    )C++");
  }
};

TEST_F(OrcaTidyTest, FieldOwnRelease) {
  std::string code = R"C++(
#include "owner.h"

    struct T {
      void Release();
    };

    struct R {
      T* t;
      ~R() { t->Release(); }
    };)C++",
              expected_changed_code = R"C++(
#include "owner.h"

    struct T {
      void Release();
    };

    struct R {
      gpos::owner<T*> t;
      ~R() { t->Release(); }
    };)C++";

  TestBeforeAfter(code, expected_changed_code);
}

TEST_F(OrcaTidyTest, Idempotence) {
  std::string code = R"C++(
#include "owner.h"

    struct T {
      void Release();
    };

    struct R {
      gpos::owner<T*> t;  // don't annotate me again
      T* t2;
      ~R() {
        t->Release();
        t2->Release();
      }
    };)C++",
              expected_changed_code = R"C++(
#include "owner.h"

    struct T {
      void Release();
    };

    struct R {
      gpos::owner<T*> t;  // don't annotate me again
      gpos::owner<T*> t2;
      ~R() {
        t->Release();
        t2->Release();
      }
    };)C++";

  TestBeforeAfter(code, expected_changed_code);
}
