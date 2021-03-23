#include "AnnotateTest.h"

namespace orca_tidy {
struct CastTest : PropagateTest {};

TEST_F(BaseTest, retCastDyncast) {
  std::string code = R"C++(
    struct R : gpos::CRefCount<R> {
      virtual ~R() {}
    };

    struct Q : R {
      static Q* Cast(R* r) { return dynamic_cast<Q*>(r); }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R : gpos::CRefCount<R> {
      virtual ~R() {}
    };

    struct Q : R {
      static gpos::cast_func<Q*> Cast(R* r) { return dynamic_cast<Q*>(r); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retCastInitDyncast) {
  std::string code = R"C++(
    struct R : gpos::CRefCount<R> {
      virtual ~R() {}
    };

    struct Q : R {
      static Q* Cast(R* r) {
        Q* q = dynamic_cast<Q*>(r);
        Assert(q != nullptr);
        return q;
      }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R : gpos::CRefCount<R> {
      virtual ~R() {}
    };

    struct Q : R {
      static gpos::cast_func<Q*> Cast(R* r) {
        Q* q = dynamic_cast<Q*>(r);
        Assert(q != nullptr);
        return q;
      }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(BaseTest, retCastNegativeCases) {
  std::string code = R"C++(
    struct R : gpos::CRefCount<R> {
      virtual ~R() {}
    };

    gpos::pointer<const R*> RetConst(gpos::pointer<const R*> r) {
      return dynamic_cast<const R*>(r);
    }
    R* Binary(int, R* r) { return dynamic_cast<R*>(r); }
    static R* r = nullptr;
    R* RetGlob(gpos::pointer<R*>) { return dynamic_cast<R*>(r); }
  )C++";

  ASSERT_EQ(format(kPreamble + code), annotateAndFormat(code));
}

TEST_F(CastTest, rewriteCallToCastFuncToUniversalCast) {
  std::string code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };
    bool F(R*);

    bool f(T* t) { return F(R::Convert(t)); }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };
    bool F(R*);

    bool f(T* t) { return F(gpos::dyn_cast<R>(t)); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(CastTest, calledInFunctionTemplateInstantiation) {
  std::string code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    bool f(T* t) {
      return F(U::Convert(t));
    }

    template bool f<R>(T*);
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    bool f(T* t) {
      return F(gpos::dyn_cast<U>(t));
    }

    template bool f<R>(T*);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(CastTest, calledInClassTemplateInstantiation) {
  std::string code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(T* t) { return F(U::Convert(t)); }
    };

    bool h(P<R>* p, T* t) { return p->g(t); }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(T* t) { return F(gpos::dyn_cast<U>(t)); }
    };

    bool h(P<R>* p, T* t) { return p->g(t); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(CastTest, calledInInstantiationButTypeIndependent) {
  std::string code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(T* t) { return F(R::Convert(t)); }
    };

    bool h(P<R>* p, T* t) { return p->g(t); }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {
      static gpos::cast_func<R*> Convert(T*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(T* t) { return F(gpos::dyn_cast<R>(t)); }
    };

    bool h(P<R>* p, T* t) { return p->g(t); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(CastTest, rewriteCStyleCastToUniversal) {
  std::string code = R"C++(
    struct R : T {};
    R* f(gpos::owner<T*> t) { return (R*)t; }
    R* g(gpos::pointer<const T*> t) { return (R*)t; }
  )C++",
              expected_changed_code = R"C++(
    struct R : T {};
    R* f(gpos::owner<T*> t) { return gpos::cast<R>(t); }
    R* g(gpos::pointer<const T*> t) { return const_cast<R*>(gpos::cast<R>(t)); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
