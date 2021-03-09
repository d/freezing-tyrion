#include "ConverterTest.h"

namespace orca_tidy {
struct Casting : ConverterTest {};

TEST_F(Casting, rewriteCallee) {
  std::string code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static gpos::cast_func<R*> Convert(Q*);
    };

    bool F(R*);

    bool f(Q* q) { return F(R::Convert(q)); }
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static R* Convert(Q*);
    };

    bool F(R*);

    bool f(Q* q) { return F(gpos::dyn_cast<R>(q)); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(Casting, calledInFunctionTemplateInstantiation) {
  std::string code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static gpos::cast_func<R*> Convert(Q*);
    };

    bool F(R*);

    template <class U>
    bool f(Q* q) {
      return F(U::Convert(q));
    }

    template bool f<R>(Q*);
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static R* Convert(Q*);
    };

    bool F(R*);

    template <class U>
    bool f(Q* q) {
      return F(gpos::dyn_cast<U>(q));
    }

    template bool f<R>(Q*);
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(Casting, calledInClassTemplateInstantiation) {
  std::string code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static gpos::cast_func<R*> Convert(Q*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(Q* q) { return F(U::Convert(q)); }
    };

    bool h(P<R>* p, Q* q) { return p->g(q); }
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static R* Convert(Q*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(Q* q) { return F(gpos::dyn_cast<U>(q)); }
    };

    bool h(P<R>* p, Q* q) { return p->g(q); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}

TEST_F(Casting, calledInInstantiationButTypeIndependent) {
  std::string code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static gpos::cast_func<R*> Convert(Q*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(Q* q) { return F(R::Convert(q)); }
    };

    bool h(P<R>* p, Q* q) { return p->g(q); }
  )C++",
              expected_changed_code = R"C++(
    struct Q {
      virtual ~Q();
    };
    struct R : Q {
      static R* Convert(Q*);
    };

    bool F(R*);

    template <class U>
    struct P {
      bool g(Q* q) { return F(gpos::dyn_cast<R>(q)); }
    };

    bool h(P<R>* p, Q* q) { return p->g(q); }
  )C++";

  auto changed_code = annotateAndFormat(std::move(code));

  ASSERT_EQ(format(kPreamble + expected_changed_code), changed_code);
}
}  // namespace orca_tidy
