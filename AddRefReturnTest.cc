#include "OrcaTidyTest.h"

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
    T* F();

    gpos::pointer<T*> F();
    gpos::owner<T*> G(gpos::owner<T*>);

    gpos::owner<T*> foo(int i, bool b) {
      static T* global_u = F();
      global_u->AddRef();
      return global_u;
    }
  )C++",
              common_add_ref = R"C++(
    T* F();

    gpos::pointer<T*> F();
    gpos::owner<T*> G(gpos::owner<T*>);

    gpos::owner<T*> foo(int i, bool b) {
      // this is actually a pointer, but the code is a bit too clever for our
      // dumb tool to recognize it
      T* ambiguous_pointer = F();

      // If we can do some fancy schmancy CFG analysis, we should see that
      // there is a code path where we return ambiguous_pointer almost "right
      // after" AddRef() (when we take the b == false branch in the
      // conditional)
      ambiguous_pointer->AddRef();
      if (b) return G(ambiguous_pointer);

      return ambiguous_pointer;
    }
  )C++",
              two_face = R"C++(
#include <cstddef>
    gpos::pointer<T*> F(int);
    gpos::owner<T*> G(int);

    gpos::owner<T*> foo(int i) {
      T* two_face;
      if (i < 42) {
        two_face = F(0);
        // pointer phase
        if (two_face != nullptr) {
          two_face->AddRef();
          return two_face;
        }
        two_face = F(1);
        if (two_face) {
          two_face->AddRef();
          return two_face;
        }
      }
      // pointers still
      two_face = F(2);
      if (NULL != two_face) {
        two_face->AddRef();
        return two_face;
      }
      // owner phase
      two_face = G(42);
      return two_face;
    }
  )C++";

  for (const auto& code : {global_var, common_add_ref, two_face}) {
    auto changed_code = annotateAndFormat(code);

    ASSERT_EQ(format(kPreamble + code), changed_code);
  }
}
}  // namespace orca_tidy