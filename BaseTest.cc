#include "OrcaTidyTest.h"

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
}  // namespace orca_tidy
