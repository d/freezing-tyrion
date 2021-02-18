#include "AnnotateTest.h"

namespace orca_tidy {
struct OutParamBase : BaseTest {};
struct OutParamProp : PropagateTest {};
TEST_F(OutParamBase, ownReleaseSafeRelease) {
  std::string code = R"C++(
    void foo(T**);

    void foo(T** ppt) { (*ppt)->Release(); }
    void bar(T** ppt) { SafeRelease(*ppt); }
  )C++",
              expected_changed_code = R"C++(
    void foo(gpos::owner<T*>*);

    void foo(gpos::owner<T*>* ppt) { (*ppt)->Release(); }
    void bar(gpos::owner<T*>* ppt) { SafeRelease(*ppt); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OutParamProp, ownNew) {
  std::string code = R"C++(
    void MakeT(T** ppt) { *ppt = new T; }
  )C++",
              expected_changed_code = R"C++(
    void MakeT(gpos::owner<T*>* ppt) {
      *ppt = new T;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OutParamProp, ownFunc) {
  std::string code = R"C++(
    gpos::owner<T*> NewT();
    void MakeT(T** ppt) { *ppt = NewT(); }
  )C++",
              expected_changed_code = R"C++(
    gpos::owner<T*> NewT();
    void MakeT(gpos::owner<T*>* ppt) { *ppt = NewT(); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OutParamProp, ownVarOwn) {
  std::string code = R"C++(
    void MakeT(T** ppt) {
      gpos::owner<T*> o = new T;
      *ppt = o;
    }
  )C++",
              expected_changed_code = R"C++(
    void MakeT(gpos::owner<T*>* ppt) {
      gpos::owner<T*> o = new T;
      *ppt = o;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OutParamProp, ownNewNegativeCases) {
  std::string code = R"C++(
    gpos::owner<U*> MakeU(gpos::owner<T*> po);
    void MakeT(T** ppt, gpos::owner<U*>* u) {
      *ppt = new T;
      *u = MakeU(*ppt);
    }
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

TEST_F(OutParamProp, Prop) {
  std::string code = R"C++(
    void MakeT(gpos::owner<T*>*);
    void foo(T** ppt) { MakeT(ppt); }

    void GetT(gpos::pointer<T*>*);
    void bar(T** ppt) { GetT(ppt); }
  )C++",
              expected_changed_code = R"C++(
    void MakeT(gpos::owner<T*>*);
    void foo(gpos::owner<T*>* ppt) { MakeT(ppt); }

    void GetT(gpos::pointer<T*>*);
    void bar(gpos::pointer<T*>* ppt) { GetT(ppt); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OutParamProp, varInit) {
  std::string code = R"C++(
    void MakeT(gpos::owner<T*>* ppt);
    void foo(T* pt) {
      MakeT(&pt);
      Sink(pt);
    }

    void GetT(gpos::pointer<T*>*);
    void bar(T* pt) { GetT(&pt); }
  )C++",
              expected_changed_code = R"C++(
    void MakeT(gpos::owner<T*>* ppt);
    void foo(gpos::owner<T*> pt) {
      MakeT(&pt);
      Sink(pt);
    }

    void GetT(gpos::pointer<T*>*);
    void bar(gpos::pointer<T*> pt) { GetT(&pt); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
