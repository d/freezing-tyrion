#include "AnnotateTest.h"

namespace orca_tidy {
TEST_F(BaseTest, varOwnInitAddRef) {
  std::string code = R"C++(
    T* F();
    void Sink(gpos::owner<T*>);

    void f() {
      T* t = F();
      t->AddRef();

      Sink(t);
    }
  )C++",
              expected_changed_code = R"C++(
    T* F();
    void Sink(gpos::owner<T*>);

    void f() {
      gpos::owner<T*> t = F();
      t->AddRef();

      Sink(t);
    }
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retPointField) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;
      T* GetT() const { return t; }
      ~R() { t->Release(); }
    };

    struct P {
      gpos::pointer<T*> p;
    };
    struct V : P {
      T* GetP() { return p; }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> t;
      gpos::pointer<T*> GetT() const { return t; }
      ~R() { t->Release(); }
    };

    struct P {
      gpos::pointer<T*> p;
    };
    struct V : P {
      gpos::pointer<T*> GetP() { return p; }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retPointFieldNegativeCases) {
  std::string code = R"C++(
    struct R {
      gpos::pointer<T*> p;
      // even though we don't AddRef, we should be conservative with the
      // assignment
      T* GetP() {
        p = new T;
        return p;
      }
    };
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

TEST_F(BaseTest, retPointCDynPtrArrSubscriptGetter) {
  std::string code = R"C++(
    struct R {
      gpos::owner<gpos::CDynamicPtrArray<T, gpos::CleanupRelease>*> tarr;

      T* GetT(gpos::ULONG i) const { return (*tarr)[i]; }
      ~R() { tarr->Release(); }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<gpos::CDynamicPtrArray<T, gpos::CleanupRelease>*> tarr;

      gpos::pointer<T*> GetT(gpos::ULONG i) const { return (*tarr)[i]; }
      ~R() { tarr->Release(); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retPointFieldQualifiers) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;

      const T* ConstQualified() { return t; }
      T const* EastConstQualified() { return t; }
      virtual T const* EastConstQualifiedVfun() { return t; }
      ~R() { t->Release(); }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::owner<T*> t;

      gpos::pointer<const T*> ConstQualified() { return t; }
      gpos::pointer<T const*> EastConstQualified() { return t; }
      virtual gpos::pointer<T const*> EastConstQualifiedVfun() { return t; }
      ~R() { t->Release(); }
    };
  )C++";

  auto changed_code = annotateAndFormat(code);

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

}  // namespace orca_tidy
