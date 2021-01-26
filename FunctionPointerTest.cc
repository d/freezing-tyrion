#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(PropagateTest, fpRet) {
  std::string code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using PfOwnT = T* (*)(int);
    typedef T* (*PfPT)();
    using PfPAnnotated = gpos::pointer<T*> (*)();

    void PO() { PfOwnT po = F; }
    void PP() { PfPT pp = &G; }
    void PPA() { PfPAnnotated pp = G; }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using PfOwnT = gpos::owner<T*> (*)(int);
    typedef gpos::pointer<T*> (*PfPT)();
    using PfPAnnotated = gpos::pointer<T*> (*)();

    void PO() { PfOwnT po = F; }
    void PP() { PfPT pp = &G; }
    void PPA() { PfPAnnotated pp = G; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, fRet) {
  std::string code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using FOwnT = T*(int);
    typedef T* FPT();

    void FpVar() {
      FOwnT* po = F;
      FPT* pp = &G;
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using FOwnT = gpos::owner<T*>(int);
    typedef gpos::pointer<T*> FPT();

    void FpVar() {
      FOwnT* po = F;
      FPT* pp = &G;
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, fRetConst) {
  std::string code = R"C++(
    gpos::pointer<const T*> G();
    typedef const T* FPT();

    void f() { FPT* pp = G; }
  )C++",
              expected_changed_code = R"C++(
    gpos::pointer<const T*> G();
    typedef gpos::pointer<const T*> FPT();

    void f() { FPT* pp = G; }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, pfStructArr) {
  std::string code = R"C++(
    gpos::owner<T*> G();
    using INT = int;
    typedef T* FPOwn();
    typedef T* (*PfPOwn)();
    struct Q {
      INT i;
      FPOwn* pf;
    };
    struct P {
      INT* l;
      PfPOwn pf;
    };

    void f() {
      Q qs[] = {
          {42, G},
      };

      P ps[] = {
          {nullptr, &G},
      };
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> G();
    using INT = int;
    typedef gpos::owner<T*> FPOwn();
    typedef gpos::owner<T*> (*PfPOwn)();
    struct Q {
      INT i;
      FPOwn* pf;
    };
    struct P {
      INT* l;
      PfPOwn pf;
    };

    void f() {
      Q qs[] = {
          {42, G},
      };

      P ps[] = {
          {nullptr, &G},
      };
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, pmfRet) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> F();
    };
    typedef T* (R::*Pmf)();
    struct Q {
      int i;
      Pmf pmf;
    };

    void f() {
      Q qs[] = {
          {42, &R::F},
      };
    }
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> F();
    };
    typedef gpos::owner<T*> (R::*Pmf)();
    struct Q {
      int i;
      Pmf pmf;
    };

    void f() {
      Q qs[] = {
          {42, &R::F},
      };
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, pfParamRet) {
  std::string code = R"C++(
    typedef T* FOwn(S*);
    void G(FOwn fown);
    gpos::owner<T*> F(gpos::pointer<S*>);

    void f() { G(F); }
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::owner<T*> FOwn(S*);
    void G(FOwn fown);
    gpos::owner<T*> F(gpos::pointer<S*>);

    void f() { G(F); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(PropagateTest, retOwnPf) {
  std::string code = R"C++(
    using FOwn = gpos::owner<T*>(S*);
    T* G(FOwn fown, S* s) { return fown(s); }
  )C++",
              expected_changed_code = R"C++(
    using FOwn = gpos::owner<T*>(S*);
    gpos::owner<T*> G(FOwn fown, S* s) { return fown(s); }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
