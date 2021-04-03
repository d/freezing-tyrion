#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertAutoRefTest : ConverterTest {};

TEST_F(ConvertAutoRefTest, autoRefInitFunc) {
  std::string code = R"C++(
    T* F();
    void f() {
      gpos::CAutoRef<T> t(F());
      Assert(t.Value());
    }
  )C++",
              expected_changed_code = R"C++(
    T* F();
    void f() {
      gpos::Ref<T> t(F());
      Assert(t.get());
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAutoRefTest, eraseUnusedAutoRef) {
  std::string code = R"C++(
    void f() {
      gpos::CAutoRef<T> a_t;
      gpos::owner<T*> ot = new T;
      a_t = ot;
    }
  )C++",
              expected_changed_code = R"C++(
    void f() {
      ;
      gpos::Ref<T> ot = new T;
      ;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
