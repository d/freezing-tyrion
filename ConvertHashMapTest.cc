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
    void g(gpos::owner<TUMap*> tumap) {
      gpos::CHashMapIter<T, U, T::UlHash, T::FEquals, gpos::CleanupRelease<T>,
                         gpos::CleanupRelease<U>>
          iter(tumap);
    }
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
    void g(gpos::Ref<TUMap> tumap) {
      gpos::UnorderedMap<
          gpos::Ref<T>, gpos::Ref<U>, gpos::RefHash<T, T::UlHash>,
          gpos::RefEq<T, T::FEquals>>::LegacyIterator iter(tumap.get());
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertHashMap, refKNullT) {
  std::string code = R"C++(
    typedef gpos::CHashMap<T, gpos::ULONG, T::UlHash, T::FEquals,
                           gpos::CleanupRelease<T>, gpos::CleanupNULL>
        TUMap;
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::UnorderedMap<gpos::Ref<T>, gpos::ULONG*,
                               gpos::RefHash<T, T::UlHash>,
                               gpos::RefEq<T, T::FEquals>>
        TUMap;
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertHashMap, nullKNullT) {
  std::string code = R"C++(
    typedef gpos::CHashMap<T, gpos::ULONG, T::UlHash, T::FEquals,
                           gpos::CleanupNULL<T>, gpos::CleanupNULL>
        TUMap;
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::UnorderedMap<const T*, gpos::ULONG*,
                               gpos::PtrHash<T, T::UlHash>,
                               gpos::PtrEqual<T, T::FEquals>>
        TUMap;
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertHashMap, nullKRefT) {
  std::string code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::CHashMap<T, U, T::UlHash, T::FEquals, gpos::CleanupNULL<T>,
                           gpos::CleanupRelease<U>>
        TUMap;
  )C++",
              expected_changed_code = R"C++(
    struct U : gpos::CRefCount<U> {};

    typedef gpos::UnorderedMap<const T*, gpos::Ref<U>,
                               gpos::PtrHash<T, T::UlHash>,
                               gpos::PtrEqual<T, T::FEquals>>
        TUMap;
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
