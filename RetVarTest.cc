#include "OrcaTidyTest.h"

namespace orca_tidy {
TEST_F(PropagateTest, retOwnVar) {
  std::string code = R"C++(
    gpos::owner<U*> global;

    // returns pointer
    U* GetGlobal() { return global; }
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
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<U*> global;

    // returns pointer
    U* GetGlobal() { return global; }
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
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
