#include "OrcaTidyTest.h"
#include "AnnotateTest.h"

namespace orca_tidy {
std::string format(const std::string& code) {
  auto style =
      clang::format::getGoogleStyle(clang::format::FormatStyle::LK_Cpp);
  style.DerivePointerAlignment = false;
  auto format_changes =
      clang::format::reformat(style, code, {{0, (unsigned int)code.size()}});

  auto changed_code =
      clang::tooling::applyAllReplacements(code, format_changes);
  // FIXME: maybe a proper error / expected style code here?
  if (!changed_code) std::terminate();
  return changed_code.get();
}

TEST_F(BaseTest, retOwnNew) {
  std::string code = R"C++(
    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    S* bar();
    gpos::owner<S*> bar();
    S* bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }

    U* implicitly_cast() { return new S; }
  )C++",
              expected_changed_code = R"C++(
    struct R {};

    R* foo() { return new R; }  // not ref counted, leave me alone
    gpos::owner<S*> bar();
    gpos::owner<S*> bar();
    gpos::owner<S*> bar() { return new S; }
    gpos::owner<S*> annotated() { return new S; }

    gpos::owner<U*> implicitly_cast() { return new S; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retOwnFunc) {
  std::string code = R"C++(
    gpos::owner<U*> F();

    gpos::owner<U*> Unannotated(int);  // leave me alone
    U* Unannotated(int);               // annotate me too
    U* Unannotated(int i) {
      if (i == 0) return F();
      return nullptr;
    }
    gpos::owner<U*> Annotated(int i)  // leave me alone
    {
      if (i == 42) return F();
      return nullptr;
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<U*> F();

    gpos::owner<U*> Unannotated(int);  // leave me alone
    gpos::owner<U*> Unannotated(int);  // annotate me too
    gpos::owner<U*> Unannotated(int i) {
      if (i == 0) return F();
      return nullptr;
    }
    gpos::owner<U*> Annotated(int i)  // leave me alone
    {
      if (i == 42) return F();
      return nullptr;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunRetUp) {
  std::string code = R"C++(
    struct Q {
      virtual U* foo();
      virtual U* bar();
    };

    struct R : Q {
      gpos::owner<U*> foo() override;
      gpos::pointer<U*> bar() override;
    };
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual gpos::owner<U*> foo();
      virtual gpos::pointer<U*> bar();
    };

    struct R : Q {
      gpos::owner<U*> foo() override;
      gpos::pointer<U*> bar() override;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunRetDown) {
  std::string code = R"C++(
    struct Q {
      virtual gpos::owner<U*> foo();
      virtual gpos::pointer<U*> bar();
    };

    struct R : Q {
      gpos::owner<U*> foo() override;
      U* bar() override;
    };

    U* Q::foo() { return nullptr; }
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual gpos::owner<U*> foo();
      virtual gpos::pointer<U*> bar();
    };

    struct R : Q {
      gpos::owner<U*> foo() override;
      gpos::pointer<U*> bar() override;
    };

    gpos::owner<U*> Q::foo() { return nullptr; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, vfunParmDown) {
  std::string code = R"C++(
    struct Q {
      virtual void foo(gpos::owner<U*>, gpos::owner<U*>, gpos::pointer<U*>);
    };

    struct R : Q {
      void foo(U* u, gpos::owner<U*> annotated, U* p) override;
    };
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual void foo(gpos::owner<U*>, gpos::owner<U*>, gpos::pointer<U*>);
    };

    struct R : Q {
      void foo(gpos::owner<U*> u, gpos::owner<U*> annotated,
               gpos::pointer<U*> p) override;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retPointOwnField) {
  std::string code = R"C++(
    gpos::pointer<T*> CalculateT();
    struct R {
      gpos::owner<T*> t;

      T* ComputeT() {
        if (!t) {
          t = CalculateT();
          t->AddRef();
        }
        return t;
      }
      ~R() { gpos::SafeRelease(t); }
    };

    struct Q : R {
      T* GetT() {
        gpos::SafeRelease(t);
        t = CalculateT();
        return t;
      }
    };
  )C++",
              expected_changed_code = R"C++(
    gpos::pointer<T*> CalculateT();
    struct R {
      gpos::owner<T*> t;

      gpos::pointer<T*> ComputeT() {
        if (!t) {
          t = CalculateT();
          t->AddRef();
        }
        return t;
      }
      ~R() { gpos::SafeRelease(t); }
    };

    struct Q : R {
      gpos::pointer<T*> GetT() {
        gpos::SafeRelease(t);
        t = CalculateT();
        return t;
      }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retPointPointField) {
  std::string code = R"C++(
    gpos::owner<T*> MakeT();
    gpos::pointer<T*> GetT();
    struct R {
      gpos::pointer<T*> t;
    };

    struct Q : R {
      T* GetT() { return t; }
      T* CreateT() {
        t = MakeT();
        return t;
      }
    };
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> MakeT();
    gpos::pointer<T*> GetT();
    struct R {
      gpos::pointer<T*> t;
    };

    struct Q : R {
      gpos::pointer<T*> GetT() { return t; }
      T* CreateT() {
        t = MakeT();
        return t;
      }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramTypeAmongRedecls) {
  std::string code = R"C++(
    // another TU annotated our params
    void F(gpos::owner<T*>, gpos::pointer<T*>);

    void F(T* p, T* q);
  )C++",
              expected_changed_code = R"C++(
    // another TU annotated our params
    void F(gpos::owner<T*>, gpos::pointer<T*>);

    void F(gpos::owner<T*> p, gpos::pointer<T*> q);
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
