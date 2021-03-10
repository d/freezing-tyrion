#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertRefArray : ConverterTest {};

TEST_F(ConvertRefArray, rewriteTypedef) {
  std::string code = R"C++(
    typedef gpos::CDynamicPtrArray<T, gpos::CleanupRelease> TArray;
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::Vector<gpos::Ref<T>> TArray;
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
