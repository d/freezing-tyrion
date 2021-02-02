#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, FieldOwnRelease) {
  std::string code = R"C++(
    struct R {
      T* t;
      T* not_released_in_dtor;
      void Cleanup() { not_released_in_dtor->Release(); }
      ~R() { t->Release(); }
    };)C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> t;
      gpos::owner<T*> not_released_in_dtor;
      void Cleanup() { not_released_in_dtor->Release(); }
      ~R() { t->Release(); }
    };)C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, FieldOwnSafeRelease) {
  std::string code = R"C++(
    struct R {
      T* t;
      T* not_released_in_dtor;
      void Cleanup() { gpos::SafeRelease(not_released_in_dtor); }
      ~R() { gpos::SafeRelease(t); }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> t;
      gpos::owner<T*> not_released_in_dtor;
      void Cleanup() { gpos::SafeRelease(not_released_in_dtor); }
      ~R() { gpos::SafeRelease(t); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, ConstQualifiersOnField) {
  std::string code = R"C++(
    struct R {
      const T* pc;
      T* const cp;
      mutable T* mp;
      mutable const T* mcp;
      const mutable T* mcp2;
      ~R() {}
    };)C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::pointer<const T*> pc;
      gpos::pointer<T*> const cp;
      mutable gpos::pointer<T*> mp;
      mutable gpos::pointer<const T*> mcp;
      mutable gpos::pointer<const T*> mcp2;
      ~R() {}
    };)C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, FieldPoint) {
  std::string code = R"C++(
    struct V {
      void Release(int*);
    };

    struct Q {
      T* t;
    };

    struct R {
      V* v;  // not ref-counted, leave me alone
      T* t;
      void bar(int* p) { v->Release(p); }
      ~R() {}
    };

    struct DtorOutOfLine {
      T* t;
      ~DtorOutOfLine();
    };

    DtorOutOfLine::~DtorOutOfLine() {}
  )C++",
              expected_changed_code = R"C++(
    struct V {
      void Release(int*);
    };

    struct Q {
      gpos::pointer<T*> t;
    };

    struct R {
      V* v;  // not ref-counted, leave me alone
      gpos::pointer<T*> t;
      void bar(int* p) { v->Release(p); }
      ~R() {}
    };

    struct DtorOutOfLine {
      gpos::pointer<T*> t;
      ~DtorOutOfLine();
    };

    DtorOutOfLine::~DtorOutOfLine() {}
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, FieldPointThroughTypedef) {
  std::string code = R"C++(
    struct Q {
      U* t;
    };

    struct R {
      U* t;
      ~R() {}
    };
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      gpos::pointer<U*> t;
    };

    struct R {
      gpos::pointer<U*> t;
      ~R() {}
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, fieldPointExcludeTemplate) {
  std::string code = R"C++(
    template <class U>
    struct R {
      T* released;
      T* not_released;

      ~R() { released->Release(); }
    };

    void f(R<T>*) {}
  )C++",
              expected_changed_code = R"C++(
    template <class U>
    struct R {
      gpos::owner<T*> released;
      T* not_released;

      ~R() { released->Release(); }
    };

    void f(R<T>*) {}
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, Idempotence) {
  std::string code = R"C++(
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

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
