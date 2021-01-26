#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, varOwnInitAddRef) {
  std::string code = R"C++(
    T* F();
    void Sink(gpos::owner<T*>);

    void f() {
      T* t = F();
      t->AddRef();

      Sink(t);
    }
  )C++",
              expected_changed_code = R"C++(
    T* F();
    void Sink(gpos::owner<T*>);

    void f() {
      gpos::owner<T*> t = F();
      t->AddRef();

      Sink(t);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
