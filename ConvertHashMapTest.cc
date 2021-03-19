#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertHashMap : ConverterTest {};

TEST_F(ConvertHashMap, refKRefT) {
  std::string code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::CHashMap<T, U, T::UlHash, T::FEquals, gpos::CleanupRelease<T>,
                           gpos::CleanupRelease<U>>
        TUMap;
  )C++",
              expected_changed_code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::UnorderedMap<gpos::Ref<T>, gpos::Ref<U>,
                               gpos::RefHash<T, T::UlHash>,
                               gpos::RefEq<T, T::FEquals>>
        TUMap;
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertHashMap, refKRefTIter) {
  std::string code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::CHashMap<T, U, T::UlHash, T::FEquals, gpos::CleanupRelease<T>,
                           gpos::CleanupRelease<U>>
        TUMap;

    typedef gpos::CHashMapIter<T, U, T::UlHash, T::FEquals,
                               gpos::CleanupRelease<T>, gpos::CleanupRelease<U>>
        TUMapIter;

    void f(gpos::owner<TUMap*> tumap) { TUMapIter iter(tumap); }
  )C++",
              expected_changed_code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::UnorderedMap<gpos::Ref<T>, gpos::Ref<U>,
                               gpos::RefHash<T, T::UlHash>,
                               gpos::RefEq<T, T::FEquals>>
        TUMap;

    typedef gpos::UnorderedMap<
        gpos::Ref<T>, gpos::Ref<U>, gpos::RefHash<T, T::UlHash>,
        gpos::RefEq<T, T::FEquals>>::LegacyIterator TUMapIter;

    void f(gpos::Ref<TUMap> tumap) { TUMapIter iter(tumap.get()); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
