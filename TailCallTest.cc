#include "AnnotateTest.h"

namespace orca_tidy {
struct TailCall : PropagateTest {};

TEST_F(TailCall, varMoveOwn) {
  std::string code = R"C++(
    struct R : T {
      R(gpos::owner<T*>);
    };

    bool F(gpos::owner<T*>);
    bool G(gpos::owner<R*>);
    gpos::pointer<T*> H(bool);

    bool f(R* r) { return G(r); }
    void g(R* r) { G(r); }

    gpos::pointer<T*> bar(R* r) { return H(F(r)); }
    bool bazz(T* t) { return G(new R(t)); }
    bool jazz(T* t, R* r) { return F(t) && H(G(r)); }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      R(gpos::owner<T*>);
    };

    bool F(gpos::owner<T*>);
    bool G(gpos::owner<R*>);
    gpos::pointer<T*> H(bool);

    bool f(gpos::owner<R*> r) { return G(std::move(r)); }
    void g(gpos::owner<R*> r) { G(std::move(r)); }

    gpos::pointer<T*> bar(gpos::owner<R*> r) { return H(F(std::move(r))); }
    bool bazz(gpos::owner<T*> t) { return G(new R(std::move(t))); }
    bool jazz(gpos::owner<T*> t, gpos::owner<R*> r) {
      return F(std::move(t)) && H(G(std::move(r)));
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, varMoveOwnCtorInitOwn) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;

      R(T* t) : t_(t) {}
    };
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::owner<T*> t_;

      R(gpos::owner<T*> t) : t_(std::move(t)) {}
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, varMoveOwnNegative) {
  std::string code = R"C++(
    struct R : T {
      R(gpos::pointer<T*>);
    };

    bool F(gpos::owner<T*>);
    bool G(gpos::owner<R*>);

    bool foo(R* added_ref) {
      added_ref->AddRef();
      G(added_ref);
      return F(added_ref);
    }

    // a reference to r cannot be replaced with a move here, because there are
    // multiple of them.
    bool fuzz(R* r) { return G(new R(r)) || F(r); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + code), changed_code);
}

TEST_F(TailCall, varPoint) {
  std::string code = R"C++(
    struct R {
      R(gpos::pointer<T*>);
    };
    bool F(gpos::pointer<T*>);
    bool H(R);
    T* Unannotated();

    bool foo(T* t, T* t2, S* s) {
      if (t) return F(t);
      if (!s) return H(R(s)) || F(s);
      return Unannotated() && F(t2);
    }
    void bar(T* t, S* s) { H(R(s)) || F(t); }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      R(gpos::pointer<T*>);
    };
    bool F(gpos::pointer<T*>);
    bool H(R);
    T* Unannotated();

    bool foo(gpos::pointer<T*> t, gpos::pointer<T*> t2, gpos::pointer<S*> s) {
      if (t) return F(t);
      if (!s) return H(R(s)) || F(s);
      return Unannotated() && F(t2);
    }
    void bar(gpos::pointer<T*> t, gpos::pointer<S*> s) { H(R(s)) || F(t); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, varPointCtorInitPoint) {
  std::string code = R"C++(
    class R {
      gpos::pointer<T*> t_;

      R(T* t) : t_(t) {}
    };
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::pointer<T*> t_;

      R(gpos::pointer<T*> t) : t_(t) {}
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, varPointNegative) {
  std::string code = R"C++(
    struct R {
      R(gpos::owner<T*>);
      template <class U>
      bool TMF(gpos::pointer<T*>);
    };
    bool F(gpos::pointer<T*>);
    bool H(R);
    bool G(T* unannotated_param);
    template <class U>
    bool TF(gpos::pointer<T*>);

    bool foo(T* t) { return F(t) && H(R(t)); }
    bool bar(T* t2) { return F(t2) || G(t2); }
    template <class U>
    bool bazz(T* t) {
      return TF<U>(t);
    }
    template <class U>
    bool jazz(T* t, R* r) {
      return r->TMF<U>(t);
    }

    struct O {
      O() = default;
      O(gpos::owner<S*>);
      O(T*);
    };
    class Q : O {
      gpos::owner<T*> t_;

     public:
      Q(gpos::owner<T*> t) : t_(t) { F(t); }
      Q(gpos::owner<T*> t, int) : O(t) { F(t); }
      Q(gpos::owner<S*> s) : O(s) { F(s); }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + code), changed_code);
}

TEST_F(TailCall, paramPoint) {
  std::string code = R"C++(
    namespace positive {
    struct R {
      R(T*);
    };
    bool F(T*, R);
    void G(T*);

    bool foo(gpos::pointer<T*> t) { return F(t, R(t)); }
    void bar(gpos::pointer<T*> t) { G(t); }
    }  // namespace positive
  )C++",
              expected_changed_code = R"C++(
    namespace positive {
    struct R {
      R(gpos::pointer<T*>);
    };
    bool F(gpos::pointer<T*>, R);
    void G(gpos::pointer<T*>);

    bool foo(gpos::pointer<T*> t) { return F(t, R(t)); }
    void bar(gpos::pointer<T*> t) { G(t); }
    }  // namespace positive
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, paramPointNegative) {
  std::string code = R"C++(
    T* Unannotated();
    template <class U, class CleanupFn>
    struct P {
      bool bar(U);
    };

    struct R {
      R(T*);
    };
    bool F(R);
    bool G(T*);

    bool foo(gpos::pointer<T*> added_ref) {
      added_ref->AddRef();
      return G(added_ref);
    }
    bool bar(gpos::pointer<T*> assigned) {
      assigned = Unannotated();
      return F(R(assigned));
    }
    bool bazz(P<T*, void> p, gpos::pointer<T*> t) { return p.bar(t); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + code), changed_code);
}

TEST_F(TailCall, paramOwn) {
  std::string code = R"C++(
    struct R {
      R(T*);
    };
    bool F(T*, R);
    void G(T*);

    bool foo(gpos::owner<T*> t, gpos::owner<T*> param) { return F(t, R(param)); }
    void bar(gpos::owner<T*> t) { G(t); }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      R(gpos::owner<T*>);
    };
    bool F(gpos::owner<T*>, R);
    void G(gpos::owner<T*>);

    bool foo(gpos::owner<T*> t, gpos::owner<T*> param) {
      return F(std::move(t), R(std::move(param)));
    }
    void bar(gpos::owner<T*> t) { G(std::move(t)); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(TailCall, paramOwnNegative) {
  std::string code = R"C++(
    template <class U, class CleanupFn>
    struct P {
      P(U u);
      bool Ok() const;
    };
    using Q = P<T*, void>;

    bool F(T*);
    bool G(T*);
    // be conservative about inferring ownership of function templates
    bool foo(gpos::owner<T*> t) { return Q{std::move(t)}.Ok(); }
    // idempotence: don't move the argument of std::move
    bool fuzz(gpos::owner<T*> t) { return Q{std::move(t)}.Ok(); }
    // t is referenced more than once, bail
    bool bar(gpos::owner<T*> t) { return F(t) || G(t); }

    struct O {
      O() = default;
      O(gpos::owner<S*>);
      O(T*);
    };
    class R : O {
      gpos::owner<T*> t_;

     public:
      R(gpos::owner<T*> t) : t_(t) { F(t); }
      R(gpos::owner<T*> t, int) : O(t) { F(t); }
      R(gpos::owner<S*> s) : O(s) { F(s); }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + code), changed_code);
}
}  // namespace orca_tidy
