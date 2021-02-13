#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, paramPointBoolFunc) {
  std::string code = R"C++(
    struct R {
      static bool foo(T* t);
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      static bool foo(gpos::pointer<T*> t);
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, paramPointerBoolFuncNegativeCases) {
  std::string code = R"C++(
    template <class U>
    struct R {
      static bool foo(U* u) {
        Assert(u);
        return false;
      }
    };
    template struct R<T>;

    struct Q {
      bool bar(T* t);
    };

    static bool baz(T* t);
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}
}  // namespace orca_tidy
