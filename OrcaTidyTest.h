#ifndef ORCATIDY__ORCATIDYTEST_H_
#define ORCATIDY__ORCATIDYTEST_H_
#include <iterator>
#include <utility>
#include "OrcaTidy.h"
#include "clang/Format/Format.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "gtest/gtest.h"

namespace orca_tidy {
class OrcaTidyTest : public ::testing::Test {
 protected:
  constexpr static const char* const kSourceFilePath = "/tmp/foo.cc";
  const orca_tidy::ActionOptions action_options_;

 public:
  explicit OrcaTidyTest(orca_tidy::ActionOptions action_options)
      : action_options_(action_options) {}

 protected:
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

  static std::string format(const std::string& code);

  std::string runToolOverCode(std::string code);

  std::string annotateAndFormat(std::string code) {
    return format(runToolOverCode(std::move(code)));
  }

 public:
};

struct BaseTest : OrcaTidyTest {
  BaseTest() : OrcaTidyTest({true, false}) {}
};

struct PropagateTest : OrcaTidyTest {
  PropagateTest() : OrcaTidyTest({false, true}) {}
};

}  // namespace orca_tidy
#endif  // ORCATIDY__ORCATIDYTEST_H_
