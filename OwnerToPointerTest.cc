#include "ConverterTest.h"

namespace orca_tidy {
struct OwnerToPointer : ConverterTest {};

TEST_F(OwnerToPointer, ret) {
  std::string code = R"C++(
    struct R {
      gpos::owner<T*> t;
      gpos::pointer<T*> GetT() { return t; }
    };
  )C++",
              expected_changed_code = R"C++(
    struct R {
      gpos::Ref<T> t;
      T* GetT() { return t.get(); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OwnerToPointer, varInitAssign) {
  std::string code = R"C++(
    class R {
      gpos::owner<T*> t_;

     public:
      ~R();
      gpos::pointer<T*> GetT() {
        gpos::pointer<T*> ret = t_;
        return ret;
      }
      void f() {
        gpos::pointer<T*> t;
        t = t_;
      }
    };
  )C++",
              expected_changed_code = R"C++(
    class R {
      gpos::Ref<T> t_;

     public:
      ~R();
      T* GetT() {
        T* ret = t_.get();
        return ret;
      }
      void f() {
        T* t;
        t = t_.get();
      }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OwnerToPointer, funcArg) {
  std::string code = R"C++(
    bool f(gpos::pointer<T*>);
    struct Q {
      Q(gpos::pointer<T*>);
    };

    template <class U>
    void e(const U* u);

    class R {
      gpos::owner<T*> t_;
      bool g() { return f(t_); }
      void h() { Q q(t_); }
      void i() { e(t_); }
    };
  )C++",
              expected_changed_code = R"C++(
    bool f(T*);
    struct Q {
      Q(T*);
    };

    template <class U>
    void e(const U* u);

    class R {
      gpos::Ref<T> t_;
      bool g() { return f(t_.get()); }
      void h() { Q q(t_.get()); }
      void i() { e(t_.get()); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OwnerToPointer, conditionalOp) {
  std::string code = R"C++(
    gpos::pointer<T*> f();
    class R {
      gpos::owner<T*> t_;
      gpos::pointer<T*> g(bool b) { return b ? t_ : f(); }
    };
  )C++",
              expected_changed_code = R"C++(
    T* f();
    class R {
      gpos::Ref<T> t_;
      T* g(bool b) { return b ? t_.get() : f(); }
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OwnerToPointer, ctorInitializer) {
  std::string code = R"C++(
    struct Q {
      Q(gpos::pointer<T*>);
    };

    struct R : Q {
      R(gpos::owner<T*> t) : Q(t) {}
    };

    class P {
      gpos::pointer<T*> t_;
      P(gpos::owner<T*> t) : t_(t) {}
    };
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      Q(T*);
    };

    struct R : Q {
      R(gpos::Ref<T> t) : Q(t.get()) {}
    };

    class P {
      T* t_;
      P(gpos::Ref<T> t) : t_(t.get()) {}
    };
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(OwnerToPointer, deref) {
  std::string code = R"C++(
    void f(gpos::pointer<T*>);
    void g(gpos::owner<T*>* po) { f(*po); }
  )C++",
              expected_changed_code = R"C++(
    void f(T*);
    void g(gpos::Ref<T>* po) { f(po->get()); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
