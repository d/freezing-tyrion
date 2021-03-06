#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, paramPointUnnamed) {
  std::string code = R"C++(
    int F(T*) { return 42; }
    void G(T*);
    void H(gpos::pointer<T*>) {}
  )C++",
              expected_changed_code = R"C++(
    int F(gpos::pointer<T*>) {
      return 42;
    }
    void G(T*);
    void H(gpos::pointer<T*>) {}
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, paramPointUnnamedConstQualified) {
  std::string code = R"C++(
    void F(const T*) {}
  )C++",
              expected_changed_code = R"C++(
    void F(gpos::pointer<const T*>) {}
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varPointUnused) {
  std::string code = R"C++(
    void f(T* t) {}
    void g(T* t __attribute__((unused))) { Assert(t != nullptr); }
  )C++",
              expected_changed_code = R"C++(
    void f(gpos::pointer<T*> t) {}
    void g(gpos::pointer<T*> t __attribute__((unused))) { Assert(t != nullptr); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointTypedefConst) {
  std::string code = R"C++(
    using PCT = const T*;
    void f(PCT t) {}
  )C++",
              expected_changed_code = R"C++(
    using PCT = const T*;
    void f(gpos::pointer<PCT> t) {}
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointPassedToPointerParam) {
  std::string code = R"C++(
    void F(gpos::pointer<T*>);

    void f(T* t) { F(t); }
  )C++",
              expected_changed_code = R"C++(
    void F(gpos::pointer<T*>);

    void f(gpos::pointer<T*> t) { F(t); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointInitAssignAnotherPointerVar) {
  std::string code = R"C++(
    bool f(T* t) {
      gpos::pointer<U*> u = t;
      return u == nullptr;
    }

    bool g(T* t) {
      gpos::pointer<U*> u;
      u = t;
      return u == nullptr;
    }
  )C++",
              expected_changed_code = R"C++(
    bool f(gpos::pointer<T*> t) {
      gpos::pointer<U*> u = t;
      return u == nullptr;
    }

    bool g(gpos::pointer<T*> t) {
      gpos::pointer<U*> u;
      u = t;
      return u == nullptr;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointAddRefAssignedToOwnerField) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;
      gpos::owner<S*> s_;
      void SetT(T* t) {
        t_ = t;
        t_->AddRef();
      }
      void SetS(S* s) {
        s->AddRef();
        s_ = s;
      }
    };
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::owner<T*> t_;
      gpos::owner<S*> s_;
      void SetT(gpos::pointer<T*> t) {
        t_ = t;
        t_->AddRef();
      }
      void SetS(gpos::pointer<S*> s) {
        s->AddRef();
        s_ = s;
      }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointNegativeCases) {
  std::string func_without_def = R"C++(
    void f(T* t);
  )C++",
              passed_addr_to_non_pointer_output_param_of_func = R"C++(
    void F(T**);
    void f(T* t) { F(&t); }
  )C++",
              passed_to_non_pointer_param_of_func = R"C++(
    void G(T*);

    void g(T* t) { G(t); }
  )C++",
              passed_to_non_pointer_param_of_ctor = R"C++(
    struct Q {
      Q(T*);
    };

    void g(T* t) { Q r(t); }
  )C++",
              passed_to_overload_expr = R"C++(
    struct R {
      template <class U>
      void TMF(gpos::pointer<T*>);
    };
    template <class U>
    void TF(gpos::pointer<T*>);

    template <class U>
    void f(T* t, R* r) {
      r->TMF<U>(t);
    }

    template <class U>
    void g(T* t) {
      TF<U>(t);
    }
  )C++",
              returned = R"C++(
    T* f(T* t) { return t; }
  )C++",
              released = R"C++(
    void f(T* t) { gpos::SafeRelease(t); }
    void g(T* t) { t->Release(); }
  )C++",
              init_assigned = R"C++(
    gpos::owner<T*> MakeT();
    gpos::owner<T*> f(T* t) {
      gpos::owner<T*> ret = t;
      if (!ret) ret = MakeT();
      return ret;
    }
    gpos::owner<T*> g(T* t) {
      gpos::owner<T*> ret;
      ret = t;
      if (!ret) ret = MakeT();
      return ret;
    }
  )C++",
              assigned_non_pointer = R"C++(
    T* F();
    void f(T* t) { t = F(); }
  )C++",
              passed_to_ctor_init = R"C++(
    T* F(T*);
    class R {
      gpos::owner<T*> t2_;
      gpos::owner<T*> t3_;
      R(T* t2, T* t3) : t2_(F(t2)), t3_(F(F(t3))) {}
    };
  )C++",
              followed_by_addref = R"C++(
    T* F(T*);
    void f(T* param) {
      T* t = F(param);
      t->AddRef();
    }
  )C++",
              auto_type = R"C++(
    T* F();
    void f() { auto t = F(); }
  )C++",
              macro = R"C++(
#define yolo(TYPE) \
      bool IsNull##TYPE(TYPE* x) { return false; }

    yolo(T);
  )C++";

  for (const auto& code : {
           func_without_def,
           passed_addr_to_non_pointer_output_param_of_func,
           passed_to_non_pointer_param_of_func,
           passed_to_non_pointer_param_of_ctor,
           passed_to_overload_expr,
           returned,
           released,
           init_assigned,
           assigned_non_pointer,
           passed_to_ctor_init,
           followed_by_addref,
           auto_type,
           macro,
       }) {
    auto changed_code = annotateAndFormat(code);

    ASSERT_EQ(format(kPreamble + code), changed_code);
  }
}
}  // namespace orca_tidy
