#include <iterator>
#include "OrcaTidy.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"

#include "gmock/gmock-matchers.h"
#include "gtest/gtest.h"

using ::testing::SizeIs;
using ::testing::StrEq;

TEST(OrcaTidyTest, FieldOwnRelease) {
  llvm::cl::OptionCategory category("my-tool options");
  const char* const sourceFilePath = "foo.cc";
  const char* argv[] = {"annotate", sourceFilePath, "--", "-xc++"};
  int argc = std::size(argv);

  clang::tooling::CommonOptionsParser parser(argc, argv, category);

  std::string code = R"C++(
    struct T {
      void Release();
    };

    struct R {
      T* t;
      ~R() { t->Release(); }
    };)C++";

  clang::tooling::RefactoringTool tool(parser.getCompilations(),
                                       parser.getSourcePathList());
  tool.mapVirtualFile(sourceFilePath, code);

  orca_tidy::AnnotateAction action(tool.getReplacements());

  int exit_code =
      tool.run(clang::tooling::newFrontendActionFactory(&action).get());

  ASSERT_EQ(0, exit_code);

  ASSERT_THAT(tool.getReplacements(), SizeIs(1));
  const auto& replacements = tool.getReplacements().begin()->second;

  auto changed_code = clang::tooling::applyAllReplacements(code, replacements);

  if (!changed_code) FAIL() << "failed applying replacements";

  auto expected_changed_code = R"C++(
    struct T {
      void Release();
    };

    struct R {
      gpos::owner<T*> t;
      ~R() { t->Release(); }
    };)C++";

  // TODO: nicer matcher that formats before comparison
  ASSERT_THAT(expected_changed_code, StrEq(*changed_code));
}
