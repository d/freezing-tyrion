#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, retPointConst) {
  std::string code = R"C++(
    const T* foo();

    template <class U>
    const T* baz(U*) {
      return nullptr;
    }
  )C++",
              expected_changed_code = R"C++(
    gpos::pointer<const T*> foo();

    template <class U>
    gpos::pointer<const T*> baz(U*) {
      return nullptr;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retPointConstNegativeCases) {
  std::string code = R"C++(
    template <class U>
    const U* bar() {
      return nullptr;
    }

    template const T* bar<T>();
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

TEST_F(BaseTest, varPointConst) {
  std::string code = R"C++(
    T* GetT();

    void foo(const T* ct);
    void bar() { const T* t = GetT(); }
    template <class K>
    void baz(const T*) {}
    template void baz<T>(const T*);
  )C++",
              expected_changed_code = R"C++(
    T* GetT();

    void foo(gpos::pointer<const T*> ct);
    void bar() { gpos::pointer<const T*> t = GetT(); }
    template <class K>
    void baz(gpos::pointer<const T*>) {}
    template void baz<T>(const T*);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, varPointConstNegativeCases) {
  std::string code = R"C++(
    template <class K>
    void bar(const K*) {}
    template void bar(const T*);
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

}  // namespace orca_tidy
