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
}  // namespace orca_tidy
