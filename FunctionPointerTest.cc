#include "OrcaTidyTest.h"

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

TEST_F(PropagateTest, fStructArr) {
  std::string code = R"C++(
    gpos::owner<T*> G();
    typedef T* FPOwn();
    struct Q {
      int i;
      FPOwn* pf;
    };

    void f() {
      Q qs[] = {
          {42, G},
      };
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> G();
    typedef gpos::owner<T*> FPOwn();
    struct Q {
      int i;
      FPOwn* pf;
    };

    void f() {
      Q qs[] = {
          {42, G},
      };
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
