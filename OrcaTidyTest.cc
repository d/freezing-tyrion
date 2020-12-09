#include <iterator>
#include "OrcaTidy.h"
#include "clang/Format/Format.h"
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

  void TestBeforeAfter(std::string code, std::string expected_changed_code) {
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
    auto style =
        clang::format::getGoogleStyle(clang::format::FormatStyle::LK_Cpp);
    auto format_changes = clang::format::reformat(
        style, *changed_code, {{0, (unsigned int)changed_code->size()}},
        kSourceFilePath);

    changed_code =
        clang::tooling::applyAllReplacements(*changed_code, format_changes);
    if (!changed_code) FAIL() << "failed formatting changes";
    code = std::move(changed_code.get());

    format_changes = clang::format::reformat(
        style, expected_changed_code,
        {{0, (unsigned int)expected_changed_code.size()}}, kSourceFilePath);

    auto formatted_expected_changed_code = clang::tooling::applyAllReplacements(
        expected_changed_code, format_changes);

    if (!formatted_expected_changed_code)
      FAIL() << "failed formatting expected code";

    expected_changed_code = std::move(formatted_expected_changed_code.get());

    // TODO: nicer matcher that formats before comparison
    ASSERT_THAT(code, StrEq(expected_changed_code));
  }

 public:
  OrcaTidyTest()
      : parser_([] {
          llvm::cl::OptionCategory category("my-tool options");
          const char* argv[] = {"annotate", kSourceFilePath, "--", "-xc++"};
          int argc = std::size(argv);

          return clang::tooling::CommonOptionsParser{argc, argv, category};
        }()),
        tool_(parser_.getCompilations(), parser_.getSourcePathList()) {
    tool_.mapVirtualFile("/tmp/CRefCount.h", R"C++(
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
    )C++");

    tool_.mapVirtualFile("/tmp/owner.h", R"C++(
#ifndef GPOS_OWNER_H
#define GPOS_OWNER_H
      namespace gpos {
      template <class T>
      using owner = T;

      template <class T>
      using pointer = T;
      }  // namespace gpos
#endif
    )C++");
  }
};

TEST_F(OrcaTidyTest, FieldOwnRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      T* t;
      ~R() { t->Release(); }
    };)C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;
      ~R() { t->Release(); }
    };)C++";

  TestBeforeAfter(code, expected_changed_code);
}

TEST_F(OrcaTidyTest, FieldOwnSafeRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      T* t;
      ~R() { gpos::SafeRelease(t); }
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;
      ~R() { gpos::SafeRelease(t); }
    };
  )C++";

  TestBeforeAfter(code, expected_changed_code);
}

TEST_F(OrcaTidyTest, ConstQualifiers) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      const T* pc;
      T* const cp;
      mutable T* mp;
      mutable const T* mcp;
      const mutable T* mcp2;
      ~R() {}
    };)C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::pointer<const T*> pc;
      gpos::pointer<T*> const cp;
      mutable gpos::pointer<T*> mp;
      mutable gpos::pointer<const T*> mcp;
      mutable gpos::pointer<const T*> mcp2;
      ~R() {}
    };)C++";

  TestBeforeAfter(code, expected_changed_code);
}

TEST_F(OrcaTidyTest, FieldPoint) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct S {
      T* t;
    };

    struct R {
      T* t;
      ~R() {}
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct S {
      gpos::pointer<T*> t;
    };

    struct R {
      gpos::pointer<T*> t;
      ~R() {}
    };
  )C++";

  TestBeforeAfter(code, expected_changed_code);
}

TEST_F(OrcaTidyTest, Idempotence) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;   // don't annotate me again
      gpos::owner<T*> t2;  // don't annotate me again
      T* t3;
      gpos::pointer<T*> t4;  // don't annotate me again
      T* t5;
      ~R() {
        t->Release();
        gpos::SafeRelease(t2);
        t3->Release();
      }
    };)C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;   // don't annotate me again
      gpos::owner<T*> t2;  // don't annotate me again
      gpos::owner<T*> t3;
      gpos::pointer<T*> t4;  // don't annotate me again
      gpos::pointer<T*> t5;
      ~R() {
        t->Release();
        gpos::SafeRelease(t2);
        t3->Release();
      }
    };)C++";

  TestBeforeAfter(code, expected_changed_code);
}
