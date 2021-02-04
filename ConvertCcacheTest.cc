#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertCcacheAndFriends : ConverterTest {};

TEST_F(ConvertCcacheAndFriends, CCacheTemplateParamInTypedef) {
  std::string code = R"C++(
    struct R {
      typedef gpos::CCache<T*, int*> TCache;
      typedef gpos::CCacheAccessor<T*, int*> TAccessor;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      typedef gpos::CCache<gpos::Ref<T>, int*> TCache;
      typedef gpos::CCacheAccessor<gpos::Ref<T>, int*> TAccessor;
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertCcacheAndFriends, CCacheTemplateParamInVar) {
  std::string code = R"C++(
    void foo(gpos::CCacheAccessor<T*, int*> accessor);
    void bar(gpos::CCache<T*, int*>* cache);
  )C++",
              expected_changed_code = R"C++(
    void foo(gpos::CCacheAccessor<gpos::Ref<T>, int*> accessor);
    void bar(gpos::CCache<gpos::Ref<T>, int*>* cache);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertCcacheAndFriends, CCacheReturnTypeTemplateParam) {
  std::string code = R"C++(
    template <class T, class K>
    gpos::CCache<T, K>* CreateCache();

    void foo() { CreateCache<T*, int*>(); }
  )C++",
              expected_changed_code = R"C++(
    template <class T, class K>
    gpos::CCache<T, K>* CreateCache();

    void foo() { CreateCache<gpos::Ref<T>, int*>(); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertCcacheAndFriends, CCacheAccessorInsertOrValOrNext) {
  std::string code = R"C++(
    using TCacheAccessor = gpos::CCacheAccessor<T*, int*>;
    void foo(TCacheAccessor accessor, int* key, T* t) {
      T* inserted = accessor.Insert(key, t);
      T* val = accessor.Val();
      T* next = accessor.Next();
      T* var;
      var = accessor.Val();
    }
  )C++",
              expected_changed_code = R"C++(
    using TCacheAccessor = gpos::CCacheAccessor<gpos::Ref<T>, int*>;
    void foo(TCacheAccessor accessor, int* key, T* t) {
      gpos::Ref<T> inserted = accessor.Insert(key, t);
      gpos::Ref<T> val = accessor.Val();
      gpos::Ref<T> next = accessor.Next();
      gpos::Ref<T> var;
      var = accessor.Val();
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
