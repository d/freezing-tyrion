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

    class R {
      gpos::owner<T*> t_;
      bool g() { return f(t_); }
      void h() { Q q(t_); }
    };
  )C++",
              expected_changed_code = R"C++(
    bool f(T*);
    struct Q {
      Q(T*);
    };

    class R {
      gpos::Ref<T> t_;
      bool g() { return f(t_.get()); }
      void h() { Q q(t_.get()); }
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
}  // namespace orca_tidy
