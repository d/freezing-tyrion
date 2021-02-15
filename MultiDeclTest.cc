#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, varPointMulti) {
  std::string code = R"C++(
    void foo() { T *one, *two; }
  )C++",
              expected_changed_code = R"C++(
    void foo() {
      gpos::pointer<T*> one, two;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varOwnMulti) {
  std::string code = R"C++(
    void foo() {
      T *one = new T, *two = new T;
      Sink(one, two);
    }
  )C++",
              expected_changed_code = R"C++(
    void foo() {
      gpos::owner<T *> one = new T, two = new T;
      Sink(one, two);
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
