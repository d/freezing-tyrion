#ifndef ORCATIDY__ORCATIDYTEST_H_
#define ORCATIDY__ORCATIDYTEST_H_
#include <iterator>
#include <utility>
#include "OrcaTidy.h"
#include "clang/Format/Format.h"
#include "clang/Tooling/Refactoring.h"
#include "gtest/gtest.h"

namespace orca_tidy {
std::string format(const std::string& code);

template <class Derived>
struct OrcaTidyTest : ::testing::Test {
  constexpr static const char* const kSourceFilePath = "/tmp/foo.cc";

  OrcaTidyTest() = default;

  std::string annotateAndFormat(std::string code) {
    return format(
        static_cast<Derived*>(this)->runToolOverCode(std::move(code)));
  }
};

}  // namespace orca_tidy
#endif  // ORCATIDY__ORCATIDYTEST_H_
