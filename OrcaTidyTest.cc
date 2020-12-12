#include <iterator>
#include <utility>
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

  static std::string format(const std::string& code) {
    auto style =
        clang::format::getGoogleStyle(clang::format::FormatStyle::LK_Cpp);
    auto format_changes = clang::format::reformat(
        style, code, {{0, (unsigned int)code.size()}}, kSourceFilePath);

    auto changed_code =
        clang::tooling::applyAllReplacements(code, format_changes);
    // FIXME: maybe a proper error / expected style code here?
    if (!changed_code) std::terminate();
    return changed_code.get();
  }

  std::string runToolOverCode(std::string code) {
    tool_.mapVirtualFile(kSourceFilePath, code);

    auto& file_to_replaces = GetFileToReplaces();
    orca_tidy::AnnotateAction action(file_to_replaces);

    int exit_code =
        tool_.run(clang::tooling::newFrontendActionFactory(&action).get());

    if (exit_code != 0) std::terminate();

    if (file_to_replaces.empty()) return code;
    const auto& replacements = file_to_replaces.begin()->second;

    auto changed_code =
        clang::tooling::applyAllReplacements(code, replacements);
    if (!changed_code) std::terminate();

    return changed_code.get();
  }

  std::string annotateAndFormat(std::string code) {
    return format(runToolOverCode(std::move(code)));
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
      T* not_released_in_dtor;
      void Cleanup() { not_released_in_dtor->Release(); }
      ~R() { t->Release(); }
    };)C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;
      gpos::owner<T*> not_released_in_dtor;
      void Cleanup() { not_released_in_dtor->Release(); }
      ~R() { t->Release(); }
    };)C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, FieldOwnSafeRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      T* t;
      T* not_released_in_dtor;
      void Cleanup() { gpos::SafeRelease(not_released_in_dtor); }
      ~R() { gpos::SafeRelease(t); }
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};

    struct R {
      gpos::owner<T*> t;
      gpos::owner<T*> not_released_in_dtor;
      void Cleanup() { gpos::SafeRelease(not_released_in_dtor); }
      ~R() { gpos::SafeRelease(t); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, ConstQualifiersOnField) {
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

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
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

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, FieldPointThroughTypedef) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      U* t;
    };

    struct R {
      U* t;
      ~R() {}
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      gpos::pointer<U*> t;
    };

    struct R {
      gpos::pointer<U*> t;
      ~R() {}
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
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

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, parmOwnRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    // leave gpos::SafeRelease alone
    template <class T>
    void gpos::SafeRelease(CRefCount<T>* t) {
      if (t) t->Release();
    }

    void OwnsParam(S* released, S*, int i);

    void OwnsParam(S* released, S* safe_released, int i) {
      if (i) {
        released->Release();
        gpos::SafeRelease(safe_released);
      }
    }

    void AlsoOwnsParam(gpos::owner<S*>, gpos::owner<S*>, int);
    void AlsoOwnsParam(gpos::owner<S*> released, S* safe_released, int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }

    void OwnsParam(S*, S*, int);  // can't change this one, too hard
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    // leave gpos::SafeRelease alone
    template <class T>
    void gpos::SafeRelease(CRefCount<T>* t) {
      if (t) t->Release();
    }

    void OwnsParam(gpos::owner<S*> released, gpos::owner<S*>, int i);

    void OwnsParam(gpos::owner<S*> released, gpos::owner<S*> safe_released, int i) {
      if (i) {
        released->Release();
        gpos::SafeRelease(safe_released);
      }
    }

    void AlsoOwnsParam(gpos::owner<S*>, gpos::owner<S*>, int);
    void AlsoOwnsParam(gpos::owner<S*> released, gpos::owner<S*> safe_released,
                       int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }

    void OwnsParam(S*, S*, int);  // can't change this one, too hard
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, varOwnRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    void foo() {
      S *s;

      s->Release();
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    void foo() {
      gpos::owner<S *> s;

      s->Release();
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, varOwnNew) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      S *s = new S;

      gpos::owner<S *> annotated = new S;
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      gpos::owner<S *> s = new S;

      gpos::owner<S *> annotated = new S;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(OrcaTidyTest, retOwnNew) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    S* bar();
    gpos::owner<S*> bar();
    S* bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    gpos::owner<S*> bar();
    gpos::owner<S*> bar();
    gpos::owner<S*> bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}
