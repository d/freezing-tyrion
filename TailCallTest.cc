#include "OrcaTidyTest.h"

namespace orca_tidy {
TEST_F(PropagateTest, varMoveOwnTailCall) {
  std::string code = R"C++(
    struct R : T {
      R(int, gpos::owner<T*>);
      R(gpos::pointer<T*>);
    };

    bool F(gpos::owner<T*>);
    bool G(gpos::owner<R*>);
    gpos::pointer<T*> H(bool);
    bool foo(int, R*, R*);

    bool foo(int i, R* r, R* added_ref) {
      T* t = nullptr;
      switch (i) {
        default:
          return F(r);
        case 42:
          return G(r);
        case 41: {
          added_ref->AddRef();
          bool b = G(added_ref);
          if (b)
            return F(added_ref);
          else
            return F(t);
        }
      }
    }

    gpos::pointer<T*> bar(R* r) { return H(F(r)); }
    bool bazz(T* t) { return G(new R(42, t)); }
    bool jazz(T* t, R* r) { return F(t) && H(G(r)); }

    // a reference to r cannot be replaced with a move here, because there are
    // multiple of them.
    bool fuzz(R* r) { return G(new R(r)) || F(r); }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      R(int, gpos::owner<T*>);
      R(gpos::pointer<T*>);
    };

    bool F(gpos::owner<T*>);
    bool G(gpos::owner<R*>);
    gpos::pointer<T*> H(bool);
    bool foo(int, gpos::owner<R*>, R*);

    bool foo(int i, gpos::owner<R*> r, R* added_ref) {
      gpos::owner<T*> t = nullptr;
      switch (i) {
        default:
          return F(std::move(r));
        case 42:
          return G(std::move(r));
        case 41: {
          added_ref->AddRef();
          bool b = G(added_ref);
          if (b)
            return F(added_ref);
          else
            return F(std::move(t));
        }
      }
    }

    gpos::pointer<T*> bar(gpos::owner<R*> r) { return H(F(std::move(r))); }
    bool bazz(gpos::owner<T*> t) { return G(new R(42, std::move(t))); }
    bool jazz(gpos::owner<T*> t, gpos::owner<R*> r) {
      return F(std::move(t)) && H(G(std::move(r)));
    }

    // a reference to r cannot be replaced with a move here, because there are
    // multiple of them.
    bool fuzz(R* r) { return G(new R(r)) || F(r); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, varPointTailCall) {
  std::string code = R"C++(
    struct R {
      R(gpos::pointer<T*>);
      R(int, gpos::owner<T*>);
    };
    bool F(gpos::pointer<T*>);
    bool H(R);
    bool G(T* unannotated_param);
    T* Unannotated();

    bool foo(S* s) {
      T* t = Unannotated();
      T* t2 = Unannotated();
      if (t) return F(t);
      if (!s) return H(R(s)) || F(s);
      return Unannotated() && F(t2);
    }

    bool bar() {
      T* t = Unannotated();
      T* t2 = Unannotated();
      if (t) return F(t) && H(R(42, t));
      return F(t2) || G(t2);
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      R(gpos::pointer<T*>);
      R(int, gpos::owner<T*>);
    };
    bool F(gpos::pointer<T*>);
    bool H(R);
    bool G(T* unannotated_param);
    T* Unannotated();

    bool foo(gpos::pointer<S*> s) {
      gpos::pointer<T*> t = Unannotated();
      gpos::pointer<T*> t2 = Unannotated();
      if (t) return F(t);
      if (!s) return H(R(s)) || F(s);
      return Unannotated() && F(t2);
    }

    bool bar() {
      T* t = Unannotated();
      T* t2 = Unannotated();
      if (t) return F(t) && H(R(42, t));
      return F(t2) || G(t2);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramPointTailCall) {
  std::string code = R"C++(
    T* Unannotated();
    template <class U, class CleanupFn>
    struct P {
      bool bar(U);
    };

    namespace positive {
    struct R {
      R(T*);
    };
    bool F(T*, R);

    bool foo(gpos::pointer<T*> t) { return F(t, R(t)); }
    }  // namespace positive

    namespace negative {
    struct R {
      R(T*);
    };
    bool F(T*, R);

    bool foo(gpos::pointer<T*> param, gpos::pointer<T*> t) {
      gpos::pointer<T*> var;
      P<T*, void> p;
      var = Unannotated();
      param->AddRef();
      return F(param, R(var)) || p.bar(t);
    }
    }  // namespace negative
  )C++",
              expected_changed_code = R"C++(
    T* Unannotated();
    template <class U, class CleanupFn>
    struct P {
      bool bar(U);
    };

    namespace positive {
    struct R {
      R(gpos::pointer<T*>);
    };
    bool F(gpos::pointer<T*>, R);

    bool foo(gpos::pointer<T*> t) { return F(t, R(t)); }
    }  // namespace positive

    namespace negative {
    struct R {
      R(T*);
    };
    bool F(T*, R);

    bool foo(gpos::pointer<T*> param, gpos::pointer<T*> t) {
      gpos::pointer<T*> var;
      P<T*, void> p;
      var = Unannotated();
      param->AddRef();
      return F(param, R(var)) || p.bar(t);
    }
    }  // namespace negative
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, paramOwnTailCall) {
  std::string code = R"C++(
    template <class U, class CleanupFn>
    struct P {
      P(U u);
      bool Ok() const;
    };
    using Q = P<T*, void>;

    namespace positive {
    struct R {
      R(T*);
    };
    bool F(T*, R);

    bool foo(gpos::owner<T*> t, gpos::owner<T*> param) { return F(t, R(param)); }
    }  // namespace positive

    namespace negative {
    bool F(T*);
    bool G(T*);
    // be conservative about inferring ownership of function templates
    bool foo(gpos::owner<T*> t) { return Q{t}.Ok(); }
    // idempotence: don't move the argument of std::move
    bool fuzz(gpos::owner<T*> t) { return Q{std::move(t)}.Ok(); }
    // t is referenced more than once, bail
    bool bar(gpos::owner<T*> t) { return F(t) || G(t); }
    }  // namespace negative
  )C++",
              expected_changed_code = R"C++(
    template <class U, class CleanupFn>
    struct P {
      P(U u);
      bool Ok() const;
    };
    using Q = P<T*, void>;

    namespace positive {
    struct R {
      R(gpos::owner<T*>);
    };
    bool F(gpos::owner<T*>, R);

    bool foo(gpos::owner<T*> t, gpos::owner<T*> param) {
      return F(std::move(t), R(std::move(param)));
    }
    }  // namespace positive

    namespace negative {
    bool F(T*);
    bool G(T*);
    // be conservative about inferring ownership of function templates
    bool foo(gpos::owner<T*> t) { return Q{std::move(t)}.Ok(); }
    // idempotence: don't move the argument of std::move
    bool fuzz(gpos::owner<T*> t) { return Q{std::move(t)}.Ok(); }
    // t is referenced more than once, bail
    bool bar(gpos::owner<T*> t) { return F(t) || G(t); }
    }  // namespace negative
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
