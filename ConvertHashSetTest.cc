#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertHashSet : ConverterTest {};

TEST_F(ConvertHashSet, refHashSet) {
  std::string code = R"C++(
    typedef gpos::CHashSet<T, T::UlHash, T::FEquals, gpos::CleanupRelease> TSet;
    typedef gpos::CHashSetIter<T, T::UlHash, T::FEquals, gpos::CleanupRelease>
        TSetIter;

    void f(gpos::owner<TSet*> tset) { TSetIter iter(tset); }
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::UnorderedSet<gpos::Ref<T>, gpos::RefHash<T, T::UlHash>,
                               gpos::RefEq<T, T::FEquals>>
        TSet;
    typedef gpos::UnorderedSet<gpos::Ref<T>, gpos::RefHash<T, T::UlHash>,
                               gpos::RefEq<T, T::FEquals>>::LegacyIterator
        TSetIter;

    void f(gpos::Ref<TSet> tset) { TSetIter iter(tset.get()); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
