#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertAutoRefTest : ConverterTest {};

TEST_F(ConvertAutoRefTest, autoRefInitFunc) {
  std::string code = R"C++(
    T* F();
    void f() { gpos::CAutoRef<T> t(F()); }
  )C++",
              expected_changed_code = R"C++(
    T* F();
    void f() { gpos::Ref<T> t(F()); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
