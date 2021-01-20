#include "OrcaTidyTest.h"

namespace orca_tidy {
std::string OrcaTidyTest::format(const std::string& code) {
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

std::string OrcaTidyTest::runToolOverCode(std::string code) {
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
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {};

    void foo() {
      R *r = new R;  // not ref-counted, leave me alone
      gpos::owner<U *> s = new U;
      gpos::owner<U *> implicitly_converted = new S;

      gpos::owner<U *> annotated = new U;
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

TEST_F(BaseTest, retPointField) {
  std::string code = R"C++(
    struct Q {};

    struct R {
      Q* s;
      gpos::owner<T*> t;
      gpos::pointer<T*> p;

      T* GetT() const { return t; }

      // even though we don't AddRef, we should be conservative with the
      // assignment
      T* GetP() {
        p = new T;
        return p;
      }
      ~R() { t->Release(); }
    };

    struct P {
      gpos::pointer<T*> p;
    };
    struct V : P {
      T* GetP() { return p; }
    };
  )C++",
              expected_changed_code = R"C++(
    struct Q {};

    struct R {
      Q* s;
      gpos::owner<T*> t;
      gpos::pointer<T*> p;

      gpos::pointer<T*> GetT() const { return t; }

      // even though we don't AddRef, we should be conservative with the
      // assignment
      T* GetP() {
        p = new T;
        return p;
      }
      ~R() { t->Release(); }
    };

    struct P {
      gpos::pointer<T*> p;
    };
    struct V : P {
      gpos::pointer<T*> GetP() { return p; }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retPointFieldQualifiers) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;

      const T* ConstQualified() { return t; }
      T const* EastConstQualified() { return t; }
      virtual T const* EastConstQualifiedVfun() { return t; }
      ~R() { t->Release(); }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> t;

      gpos::pointer<const T*> ConstQualified() { return t; }
      gpos::pointer<T const*> EastConstQualified() { return t; }
      virtual gpos::pointer<T const*> EastConstQualifiedVfun() { return t; }
      ~R() { t->Release(); }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
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

TEST_F(PropagateTest, varOwnInitAssignOwnFunc) {
  std::string code = R"C++(
    gpos::owner<T *> MakeT(int);
    gpos::pointer<T *> GetT();

    void foo(int i, int j) {
      // multiple declarations here, leave them alone
      T *a = MakeT(i), *b = MakeT(j);

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
      // multiple declarations here, leave them alone
      T *a = MakeT(i), *b = MakeT(j);

      gpos::owner<T *> var_init_own_func = MakeT(i);
      gpos::owner<T *> var_assign_own_func;
      var_assign_own_func = MakeT(i);
      T *var_init_point_func = GetT();
      gpos::owner<S *> var_init_cast_own_func = static_cast<S *>(MakeT(i));

      Sink(var_init_own_func, var_assign_own_func, var_init_point_func,
           var_init_cast_own_func);
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
        Sink(var_assign_own_func, var_init_point_func, var_init_own_func);
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
      Sink(var_assign_own_func, var_init_own_func);
    }
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
}  // namespace orca_tidy
