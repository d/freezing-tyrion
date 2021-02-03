#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertCcacheAndFriends : ConverterTest {};

TEST_F(ConvertCcacheAndFriends, CCacheTemplateParam) {
  std::string code = R"C++(
    struct R {
      typedef gpos::CCache<T*, int*> TCache;
    };

    struct Q {
      using TCache = gpos::CCache<T*, int*>;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      typedef gpos::CCache<gpos::Ref<T>, int*> TCache;
    };

    struct Q {
      using TCache = gpos::CCache<gpos::Ref<T>, int*>;
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertCcacheAndFriends, CCacheAccessorTemplateParam) {
  std::string code = R"C++(
    struct R {
      typedef gpos::CCacheAccessor<T*, int*> TCache;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      typedef gpos::CCacheAccessor<gpos::Ref<T>, int*> TCache;
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
