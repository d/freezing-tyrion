#include <iterator>
#include <utility>
#include "OrcaTidy.h"
#include "clang/Format/Format.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"

#include "gtest/gtest.h"

class OrcaTidyTest : public ::testing::Test {
 protected:
  constexpr static const char* const kSourceFilePath = "/tmp/foo.cc";
  const orca_tidy::ActionOptions action_options_;

 public:
  OrcaTidyTest(orca_tidy::ActionOptions action_options)
      : action_options_(action_options) {}

 protected:
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
      namespace gpos {
      template <class T>
      using owner = T;

      template <class T>
      using pointer = T;
      }  // namespace gpos
#endif
    )C++";

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

    auto changed_code =
        clang::tooling::applyAllReplacements(code, replacements);
    if (!changed_code) std::terminate();

    return changed_code.get();
  }

  std::string annotateAndFormat(std::string code) {
    return format(runToolOverCode(std::move(code)));
  }

 public:
};

struct BaseTest : OrcaTidyTest {
  BaseTest() : OrcaTidyTest({true, false}) {}
};

struct PropagateTest : OrcaTidyTest {
  PropagateTest() : OrcaTidyTest({false, true}) {}
};

TEST_F(BaseTest, FieldOwnRelease) {
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

TEST_F(BaseTest, FieldOwnSafeRelease) {
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

TEST_F(BaseTest, ConstQualifiersOnField) {
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

TEST_F(BaseTest, FieldPoint) {
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

TEST_F(BaseTest, FieldPointThroughTypedef) {
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

TEST_F(BaseTest, Idempotence) {
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

TEST_F(BaseTest, parmOwnRelease) {
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

TEST_F(BaseTest, parmVfunOwnRelease) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    struct R {
      virtual void OwnsParam(S*, gpos::owner<S*>, int);
    };

    struct U : R {
      void OwnsParam(S* released, S*, int i) override;
    };

    void U::OwnsParam(S* released, S* safe_released, int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    struct R {
      virtual void OwnsParam(gpos::owner<S*>, gpos::owner<S*>, int);
    };

    struct U : R {
      void OwnsParam(gpos::owner<S*> released, gpos::owner<S*>, int i) override;
    };

    void U::OwnsParam(gpos::owner<S*> released, gpos::owner<S*> safe_released,
                      int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnRelease) {
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

TEST_F(BaseTest, varOwnInitNew) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct U : S {};
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      S *s = new S;
      S *implicitly_converted = new U;

      gpos::owner<S *> annotated = new S;
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;
    struct U : S {};
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      gpos::owner<S *> s = new S;
      gpos::owner<S *> implicitly_converted = new U;

      gpos::owner<S *> annotated = new S;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnAssignNew) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;
    struct S : U {};
    struct R {};

    void foo() {
      R *r;
      r = new R;  // not ref-counted, leave me alone
      S *s;
      s = new S;

      U *implicitly_converted;
      implicitly_converted = new S;
      gpos::owner<S *> annotated;
      annotated = new S;
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;
    struct S : U {};
    struct R {};

    void foo() {
      R *r;
      r = new R;  // not ref-counted, leave me alone
      gpos::owner<S *> s;
      s = new S;

      gpos::owner<U *> implicitly_converted;
      implicitly_converted = new S;
      gpos::owner<S *> annotated;
      annotated = new S;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(BaseTest, retOwnNew) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S : U {};

    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    S* bar();
    gpos::owner<S*> bar();
    S* bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }

    U* implicitly_cast() { return new S; }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S : U {};

    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    gpos::owner<S*> bar();
    gpos::owner<S*> bar();
    gpos::owner<S*> bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }

    gpos::owner<U*> implicitly_cast() { return new S; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retOwnVar) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    S* Unannotated();               // annotate me too
    gpos::owner<S*> Unannotated();  // leave me alone

    gpos::owner<S*> Annotated()  // leave me alone
    {
      gpos::owner<S*> o;

      return o;
    }

    S* Unannotated() {
      gpos::owner<S*> o;

      return o;
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    gpos::owner<S*> Unannotated();  // annotate me too
    gpos::owner<S*> Unannotated();  // leave me alone

    gpos::owner<S*> Annotated()  // leave me alone
    {
      gpos::owner<S*> o;

      return o;
    }

    gpos::owner<S*> Unannotated() {
      gpos::owner<S*> o;

      return o;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retOwnFunc) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    gpos::owner<S*> F();

    gpos::owner<S*> Unannotated(int);  // leave me alone
    S* Unannotated(int);               // annotate me too
    S* Unannotated(int i) {
      if (i == 0) return F();
      return nullptr;
    }
    gpos::owner<S*> Annotated(int i)  // leave me alone
    {
      if (i == 42) return F();
      return nullptr;
    }
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using S = T;

    gpos::owner<S*> F();

    gpos::owner<S*> Unannotated(int);  // leave me alone
    gpos::owner<S*> Unannotated(int);  // annotate me too
    gpos::owner<S*> Unannotated(int i) {
      if (i == 0) return F();
      return nullptr;
    }
    gpos::owner<S*> Annotated(int i)  // leave me alone
    {
      if (i == 42) return F();
      return nullptr;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunRetUp) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual U* foo();
    };

    struct R : S {
      gpos::owner<U*> foo() override;
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual gpos::owner<U*> foo();
    };

    struct R : S {
      gpos::owner<U*> foo() override;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunRetDown) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual gpos::owner<U*> foo();
    };

    struct R : S {
      U* foo() override;
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual gpos::owner<U*> foo();
    };

    struct R : S {
      gpos::owner<U*> foo() override;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunParmDown) {
  std::string code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual void foo(gpos::owner<U*>, gpos::owner<U*>);
    };

    struct R : S {
      void foo(U* u, gpos::owner<U*> annotated) override;
    };
  )C++",
              expected_changed_code = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;

    struct S {
      virtual void foo(gpos::owner<U*>, gpos::owner<U*>);
    };

    struct R : S {
      void foo(gpos::owner<U*> u, gpos::owner<U*> annotated) override;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(expected_changed_code), changed_code);
}
