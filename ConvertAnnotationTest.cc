#include "ConverterTest.h"

namespace orca_tidy {
struct ConvertAnnotation : ConverterTest {};

TEST_F(ConvertAnnotation, pointerToRaw) {
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

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, constPointerToRaw) {
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

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, ownerToRef) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::Ref<T> t;
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, funcRet) {
  std::string code = R"C++(
    struct S : T {};

    gpos::pointer<T*> f();
    gpos::pointer<const T*> g();
    gpos::owner<T*> h();
    gpos::cast_func<S*> i(T*);
  )C++",
              expected_changed_code = R"C++(
    struct S : T {};

    T* f();
    const T* g();
    gpos::Ref<T> h();
    S* i(T*);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, var) {
  std::string code = R"C++(
    gpos::owner<T*> MakeT();
    void e() { gpos::pointer<T*> p1, p2; }
    void f(gpos::owner<T*> o, gpos::pointer<T*> p);
    void g() { gpos::leaked<T*> l = MakeT(); }
    void h(gpos::owner<T*>* po, gpos::pointer<T*>* pp);
  )C++",
              expected_changed_code = R"C++(
    gpos::Ref<T> MakeT();
    void e() { T *p1, *p2; }
    void f(gpos::Ref<T> o, T* p);
    void g() { gpos::Ref<T> l = MakeT(); }
    void h(gpos::Ref<T>* po, T** pp);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, eraseRelease) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;
      ~R() { t_->Release(); }
    };

    gpos::owner<T*> MakeT();
    void f(gpos::owner<T*>* po) {
      gpos::owner<T*> t = MakeT();
      gpos::SafeRelease(std::move(t));
      gpos::SafeRelease(*po);
    }
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::Ref<T> t_;
      ~R() { ; }
    };

    gpos::Ref<T> MakeT();
    void f(gpos::Ref<T>* po) {
      gpos::Ref<T> t = MakeT();
      ;
      ;
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, eraseAddRef) {
  std::string code = R"C++(
    void F(gpos::owner<T*>);

    void f(gpos::pointer<T*> t) {
      t->AddRef();
      return F(t);
    }
  )C++",
              expected_changed_code = R"C++(
    void F(gpos::Ref<T>);

    void f(T* t) {
      ;
      return F(t);
    }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, typedefFRet) {
  std::string code = R"C++(
    typedef gpos::owner<T*>(PFo)();
    typedef gpos::pointer<T*>(PFp)();
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::Ref<T>(PFo)();
    typedef T*(PFp)();
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, typedefPfRet) {
  std::string code = R"C++(
    typedef gpos::owner<T*> (*PFo)();
    typedef gpos::pointer<T*> (*PFp)();
  )C++",
              expected_changed_code = R"C++(
    typedef gpos::Ref<T> (*PFo)();
    typedef T* (*PFp)();
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(ConvertAnnotation, typedefPmfRet) {
  std::string code = R"C++(
    struct R {};
    typedef gpos::owner<T*> (R::*PFo)();
    typedef gpos::pointer<T*> (R::*PFp)();
  )C++",
              expected_changed_code = R"C++(
    struct R {};
    typedef gpos::Ref<T> (R::*PFo)();
    typedef T* (R::*PFp)();
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
