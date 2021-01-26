#ifndef ORCATIDY__ANNOTATETEST_H_
#define ORCATIDY__ANNOTATETEST_H_
#include "OrcaTidyTest.h"
namespace orca_tidy {
class AnnotateTest : public OrcaTidyTest<AnnotateTest> {
 public:
  explicit AnnotateTest(ActionOptions action_options)
      : action_options_(action_options) {}

  constexpr static const char* const kPreamble = R"C++(
#include "CRefCount.h"
#include "owner.h"

    struct T : gpos::CRefCount<T> {};
    using U = T;
    struct S : U {};

    void Assert(bool);
    template <class T1, class T2>
    void Sink(T1, T2);
    template <class T1, class T2, class T3>
    void Sink(T1, T2, T3);
    template <class T1, class T2, class T3, class T4>
    void Sink(T1, T2, T3, T4);
  )C++";

  std::string runToolOverCode(std::string code);

 protected:
  const orca_tidy::ActionOptions action_options_;
};

struct BaseTest : AnnotateTest {
  BaseTest() : AnnotateTest({true, false}) {}
};

struct PropagateTest : AnnotateTest {
  PropagateTest() : AnnotateTest({false, true}) {}
};

}  // namespace orca_tidy
#endif  // ORCATIDY__ANNOTATETEST_H_
