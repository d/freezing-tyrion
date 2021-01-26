#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, retOwnAddRefReturnVar) {
  std::string code = R"C++(
    S *global;
    U *GetT();
    void F(U *);

    U *GetT() {
      global->AddRef();
      return global;
    }

    U *NotSure(gpos::pointer<S *> s) {
      s->AddRef();
      F(s);
      return s;
    }
  )C++",
              expected_changed_code = R"C++(
    S *global;
    gpos::owner<U *> GetT();
    void F(U *);

    gpos::owner<U *> GetT() {
      global->AddRef();
      return global;
    }

    U *NotSure(gpos::pointer<S *> s) {
      s->AddRef();
      F(s);
      return s;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retOwnAddRefReturnField) {
  std::string code = R"C++(
    struct R {
      gpos::pointer<U *> u;
      S *GetS();
      U *NotSure();
    };

    void F(U *);

    S *R::GetS() {
      u->AddRef();
      return static_cast<S *>(u);
    }

    U *R::NotSure() {
      u->AddRef();
      F(u);
      return u;
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::pointer<U *> u;
      gpos::owner<S *> GetS();
      U *NotSure();
    };

    void F(U *);

    gpos::owner<S *> R::GetS() {
      u->AddRef();
      return static_cast<S *>(u);
    }

    U *R::NotSure() {
      u->AddRef();
      F(u);
      return u;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varPointAddRefReturn) {
  std::string code = R"C++(
    U *F();

    gpos::owner<S *> foo(int i, bool b, S *param) {
      U *u = F();
      if (i < 42) {
        param->AddRef();
        return param;
      }
      u->AddRef();
      return static_cast<S *>(u);
    }
  )C++",
              expected_changed_code = R"C++(
    U *F();

    gpos::owner<S *> foo(int i, bool b, gpos::pointer<S *> param) {
      gpos::pointer<U *> u = F();
      if (i < 42) {
        param->AddRef();
        return param;
      }
      u->AddRef();
      return static_cast<S *>(u);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, parmVfunPointAddRefReturn) {
  std::string code = R"C++(
    struct R {
      virtual U* OwnsParam(U*, gpos::pointer<U*>, int);
    };

    struct Q : R {
      gpos::owner<U*> OwnsParam(U* p, U*, int i) override;
    };

    gpos::owner<U*> Q::OwnsParam(U* p, U* annotated_in_base, int i) {
      if (i) {
        p->AddRef();
        return p;
      } else {
        annotated_in_base->AddRef();
        return annotated_in_base;
      }
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      virtual U* OwnsParam(gpos::pointer<U*>, gpos::pointer<U*>, int);
    };

    struct Q : R {
      gpos::owner<U*> OwnsParam(gpos::pointer<U*> p, gpos::pointer<U*>,
                                int i) override;
    };

    gpos::owner<U*> Q::OwnsParam(gpos::pointer<U*> p,
                                 gpos::pointer<U*> annotated_in_base, int i) {
      if (i) {
        p->AddRef();
        return p;
      } else {
        annotated_in_base->AddRef();
        return annotated_in_base;
      }
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varPointAddRefReturnNegativeCases) {
  std::string global_var = R"C++(
    gpos::pointer<T*> F();

    gpos::owner<T*> foo(int i, bool b) {
      static T* global_u = F();
      global_u->AddRef();
      return global_u;
    }
  )C++",
              two_face = R"C++(
#include <cstddef>
    gpos::pointer<T*> F(int);
    gpos::owner<T*> G(int);

    gpos::owner<T*> foo() {
      T* two_face = F(0);
      // pointer phase
      if (two_face) {
        two_face->AddRef();
        return two_face;
      }
      // owner phase
      two_face = G(42);
      return two_face;
    }
  )C++",
              init_addref_return = R"C++(
    T* F();
    gpos::owner<T*> foo() {
      gpos::owner<T*> t = F();
      t->AddRef();
      return t;
    }
  )C++";

  for (const auto& code : {global_var, two_face, init_addref_return}) {
    auto changed_code = annotateAndFormat(code);

    ASSERT_EQ(format(kPreamble + code), changed_code);
  }
}
}  // namespace orca_tidy
