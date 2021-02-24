#include "AnnotateTest.h"

namespace orca_tidy {
struct RetVarTest : PropagateTest {};

TEST_F(RetVarTest, retOwnVar) {
  std::string code = R"C++(
    void F(gpos::pointer<U*>);
    U* Unannotated();               // annotate me too
    gpos::owner<U*> Unannotated();  // leave me alone

    gpos::owner<U*> Annotated()  // leave me alone
    {
      gpos::owner<U*> o;

      return o;
    }

    U* Unannotated() {
      gpos::owner<U*> o;

      return o;
    }

    template <class V>
    T* InstantiationIndependent(gpos::owner<T*> t) {
      F(t);
      return t;
    }
    void foo() { InstantiationIndependent<T>(nullptr); }
  )C++",
              expected_changed_code = R"C++(
    void F(gpos::pointer<U*>);
    gpos::owner<U*> Unannotated();  // annotate me too
    gpos::owner<U*> Unannotated();  // leave me alone

    gpos::owner<U*> Annotated()  // leave me alone
    {
      gpos::owner<U*> o;

      return o;
    }

    gpos::owner<U*> Unannotated() {
      gpos::owner<U*> o;

      return o;
    }

    template <class V>
    gpos::owner<T*> InstantiationIndependent(gpos::owner<T*> t) {
      F(t);
      return t;
    }
    void foo() { InstantiationIndependent<T>(nullptr); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(RetVarTest, NegativeCases) {
  std::string code = R"C++(
    gpos::owner<U*> global;
    U* GetGlobal() { return global; }
    U* GetStatic() {
      static gpos::owner<S*> s;
      return s;
    }

    bool F(gpos::owner<S*> s);
    U* StealOwnership(gpos::owner<S*> s) {
      // we "transferred" ownership to F before returning s
      if (!F(s)) return nullptr;
      return s;
    }

    struct R {
      R(gpos::pointer<T*> t);
    };
    template <class V>
    T* CtorParens(gpos::owner<T*> t) {
      V v(t);
      return t;
    }

    template <class V>
    T* CtorBraces(gpos::owner<T*> t) {
      V v{t};
      return t;
    }

    void foo(gpos::owner<T*> t) {
      CtorParens<R>(t);
      CtorBraces<R>(t);
      ;
    }
  )C++";
  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + code), changed_code);
}

}  // namespace orca_tidy
