#include "AnnotateTest.h"

namespace orca_tidy {
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
}  // namespace orca_tidy
