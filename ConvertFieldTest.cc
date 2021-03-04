#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertField : ConverterTest {};

TEST_F(ConvertField, pointerToRaw) {
  std::string code = R"C++(
    struct R {
      gpos::pointer<T*> t;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      T* t;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertField, constPointerToRaw) {
  std::string code = R"C++(
    struct R {
      gpos::pointer<const T*> ct;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      const T* ct;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertField, ownerToRef) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      Ref<T> t;
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
