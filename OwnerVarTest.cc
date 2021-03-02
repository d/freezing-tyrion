#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, parmOwnRelease) {
  std::string code = R"C++(
    // leave gpos::SafeRelease alone
    template <class T>
    void gpos::SafeRelease(CRefCount<T>* t) {
      if (t) t->Release();
    }

    void OwnsParam(U* released, U*, int i);

    void OwnsParam(U* released, U* safe_released, int i) {
      if (i) {
        released->Release();
        gpos::SafeRelease(safe_released);
      }
    }

    void AlsoOwnsParam(gpos::owner<U*>, gpos::owner<U*>, int);
    void AlsoOwnsParam(gpos::owner<U*> released, U* safe_released, int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }

    void OwnsParam(gpos::owner<U*>, gpos::owner<U*>, int);
  )C++",
              expected_changed_code = R"C++(
    // leave gpos::SafeRelease alone
    template <class T>
    void gpos::SafeRelease(CRefCount<T>* t) {
      if (t) t->Release();
    }

    void OwnsParam(gpos::owner<U*> released, gpos::owner<U*>, int i);

    void OwnsParam(gpos::owner<U*> released, gpos::owner<U*> safe_released, int i) {
      if (i) {
        released->Release();
        gpos::SafeRelease(safe_released);
      }
    }

    void AlsoOwnsParam(gpos::owner<U*>, gpos::owner<U*>, int);
    void AlsoOwnsParam(gpos::owner<U*> released, gpos::owner<U*> safe_released,
                       int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }

    void OwnsParam(gpos::owner<U*>, gpos::owner<U*>, int);
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, parmVfunOwnRelease) {
  std::string code = R"C++(
    struct R {
      virtual void OwnsParam(U*, gpos::owner<U*>, int);
    };

    struct Q : R {
      void OwnsParam(U* released, U*, int i) override;
    };

    void Q::OwnsParam(U* released, U* safe_released, int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      virtual void OwnsParam(gpos::owner<U*>, gpos::owner<U*>, int);
    };

    struct Q : R {
      void OwnsParam(gpos::owner<U*> released, gpos::owner<U*>, int i) override;
    };

    void Q::OwnsParam(gpos::owner<U*> released, gpos::owner<U*> safe_released,
                      int i) {
      if (i) {
        released->Release();
      } else {
        gpos::SafeRelease(safe_released);
      }
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnRelease) {
  std::string code = R"C++(
    extern U* global_var;

    U* global_var;

    void foo() {
      U* s;

      s->Release();
    }

    void Shutdown() { global_var->Release(); }
  )C++",
              expected_changed_code = R"C++(
    extern gpos::owner<U*> global_var;

    gpos::owner<U*> global_var;

    void foo() {
      gpos::owner<U*> s;

      s->Release();
    }

    void Shutdown() { global_var->Release(); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnInitNew) {
  std::string code = R"C++(
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      U *s = new U;
      U *implicitly_converted = new S;

      gpos::owner<U *> annotated = new U;
      Sink(s, implicitly_converted, annotated);
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      gpos::owner<U *> s = new U;
      gpos::owner<U *> implicitly_converted = new S;

      gpos::owner<U *> annotated = new U;
      Sink(s, implicitly_converted, annotated);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnAssignNew) {
  std::string code = R"C++(
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

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, paramOwnAssignNew) {
  std::string code = R"C++(
    struct R {
      virtual void foo(R *, S *, U *, gpos::owner<S *>);
    };

    struct Q : R {
      void foo(R *, S *, U *, gpos::owner<S *>) override;
    };

    void Q::foo(R *r, S *s, U *implicitly_converted, gpos::owner<S *> annotated) {
      r = new R;  // not ref-counted, leave me alone
      s = new S;

      implicitly_converted = new S;
      annotated = new S;
    }

    void bar(U *u);

    void bar(U *u) { u = new S; }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      virtual void foo(R *, gpos::owner<S *>, gpos::owner<U *>, gpos::owner<S *>);
    };

    struct Q : R {
      void foo(R *, gpos::owner<S *>, gpos::owner<U *>, gpos::owner<S *>) override;
    };

    void Q::foo(R *r, gpos::owner<S *> s, gpos::owner<U *> implicitly_converted,
                gpos::owner<S *> annotated) {
      r = new R;  // not ref-counted, leave me alone
      s = new S;

      implicitly_converted = new S;
      annotated = new S;
    }

    void bar(gpos::owner<U *> u);

    void bar(gpos::owner<U *> u) { u = new S; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varOwnInitAssignOwnFunc) {
  std::string code = R"C++(
    gpos::owner<T *> MakeT(int);
    gpos::pointer<T *> GetT();

    void foo(int i, int j) {
      T *var_init_own_func = MakeT(i);
      T *var_assign_own_func;
      var_assign_own_func = MakeT(i);
      T *var_init_point_func = GetT();
      S *var_init_cast_own_func = static_cast<S *>(MakeT(i));

      Sink(var_init_own_func, var_assign_own_func, var_init_point_func,
           var_init_cast_own_func);
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T *> MakeT(int);
    gpos::pointer<T *> GetT();

    void foo(int i, int j) {
      gpos::owner<T *> var_init_own_func = MakeT(i);
      gpos::owner<T *> var_assign_own_func;
      var_assign_own_func = MakeT(i);
      T *var_init_point_func = GetT();
      gpos::owner<S *> var_init_cast_own_func = static_cast<S *>(MakeT(i));

      Sink(std::move(var_init_own_func), std::move(var_assign_own_func),
           var_init_point_func, std::move(var_init_cast_own_func));
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramOwnInitAssignOwnFunc) {
  std::string code = R"C++(
    gpos::owner<T *> MakeT(int i = 42);
    gpos::pointer<T *> GetT();

    struct R {
      void foo(int i, int j, T *var_assign_own_func,
               T *var_init_point_func = GetT(),
               T *var_init_own_func = MakeT()) {
        var_assign_own_func = MakeT(i);
        Sink(var_assign_own_func, var_init_point_func, var_init_own_func);
      }

      virtual void bar(T *var_assign_own_func, T *var_init_own_func = MakeT());
    };

    struct Q : R {
      void bar(T *var_assign_own_func, T *var_init_own_func = MakeT()) override;
    };

    void Q::bar(T *var_assign_own_func, T *var_init_own_func) {
      var_assign_own_func = MakeT(0);
      Sink(var_assign_own_func, var_init_own_func);
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T *> MakeT(int i = 42);
    gpos::pointer<T *> GetT();

    struct R {
      void foo(int i, int j, gpos::owner<T *> var_assign_own_func,
               T *var_init_point_func = GetT(),
               gpos::owner<T *> var_init_own_func = MakeT()) {
        var_assign_own_func = MakeT(i);
        Sink(std::move(var_assign_own_func), var_init_point_func,
             std::move(var_init_own_func));
      }

      virtual void bar(gpos::owner<T *> var_assign_own_func,
                       gpos::owner<T *> var_init_own_func = MakeT());
    };

    struct Q : R {
      void bar(gpos::owner<T *> var_assign_own_func,
               gpos::owner<T *> var_init_own_func = MakeT()) override;
    };

    void Q::bar(gpos::owner<T *> var_assign_own_func,
                gpos::owner<T *> var_init_own_func) {
      var_assign_own_func = MakeT(0);
      Sink(std::move(var_assign_own_func), std::move(var_init_own_func));
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramOwnNew) {
  std::string code = R"C++(
    template <class U, class CleanupFn>
    struct R {
      R(T*);
      void jazz(U*);
    };

    struct Q {
      Q(T*);
    };

    void foo(T*, T*);

    void bar() {
      R<T, void> r(new T);
      Q q(new T);
      r.jazz(new S);
      return foo(new T, new S);
    }
  )C++",
              expected_changed_code = R"C++(
    template <class U, class CleanupFn>
    struct R {
      R(T*);
      void jazz(U*);
    };

    struct Q {
      Q(gpos::owner<T*>);
    };

    void foo(gpos::owner<T*>, gpos::owner<T*>);

    void bar() {
      R<T, void> r(new T);
      Q q(new T);
      r.jazz(new S);
      return foo(new T, new S);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramOwnFunc) {
  std::string code = R"C++(
    template <class U, class CleanupFn>
    struct R {
      R(T*);
      void jazz(U*);
    };

    struct Q {
      Q(T*);
    };

    gpos::owner<T*> F();
    gpos::owner<S*> G();

    void foo(T*, T*);

    void bar() {
      R<T, void> r(F());
      Q q(G());
      r.jazz(G());
      return foo(F(), G());
    }
  )C++",
              expected_changed_code = R"C++(
    template <class U, class CleanupFn>
    struct R {
      R(T*);
      void jazz(U*);
    };

    struct Q {
      Q(gpos::owner<T*>);
    };

    gpos::owner<T*> F();
    gpos::owner<S*> G();

    void foo(gpos::owner<T*>, gpos::owner<T*>);

    void bar() {
      R<T, void> r(F());
      Q q(G());
      r.jazz(G());
      return foo(F(), G());
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, ctorParamOwnInitField) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;

     public:
      R(T* t) : t_(t) {}
      ~R();
    };
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::owner<T*> t_;

     public:
      R(gpos::owner<T*> t) : t_(t) {}
      ~R();
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, ctorParamOwnInitFieldNegativeCases) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;

     public:
      R(T* t) : t_(t) { t->AddRef(); }
      ~R();
    };
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

TEST_F(PropagateTest, ctorParamOwnInitBase) {
  std::string code = R"C++(
    struct R {
      R(gpos::owner<T*>);
    };

    struct Q : R {
      Q(T* t) : R(t) {}
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      R(gpos::owner<T*>);
    };

    struct Q : R {
      Q(gpos::owner<T*> t) : R(t) {}
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, ctorParamOwnInitBaseNegative) {
  std::string code = R"C++(
    struct R {
      R(gpos::owner<T*>);
    };

    struct Q : R {
      Q(T* t) : R(t) { t->AddRef(); }
    };
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

}  // namespace orca_tidy
