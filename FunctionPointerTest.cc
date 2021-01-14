#include "OrcaTidyTest.h"

namespace orca_tidy {
TEST_F(PropagateTest, fpRet) {
  std::string code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using PfOwnT = T* (*)(int);
    typedef T* (*PfPT)();

    void FpVar() {
      PfOwnT po = F;
      PfPT pp = &G;
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> F(int);
    gpos::pointer<T*> G();
    using PfOwnT = gpos::owner<T*> (*)(int);
    typedef gpos::pointer<T*> (*PfPT)();

    void FpVar() {
      PfOwnT po = F;
      PfPT pp = &G;
    }
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
}  // namespace orca_tidy
