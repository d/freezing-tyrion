#include "OrcaTidy.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/YAMLTraits.h"

namespace orca_tidy {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

using FileToReplacements = std::map<std::string, clang::tooling::Replacements>;

class AnnotateASTConsumer : public clang::ASTConsumer {
  FileToReplacements& replacements_;

 public:
  explicit AnnotateASTConsumer(FileToReplacements& replacements)
      : replacements_(replacements) {}

  void HandleTranslationUnit(clang::ASTContext& ast_context) override {
    const auto owner_decl = typeAliasTemplateDecl(hasName("::gpos::owner"));
    const auto is_owner_type = hasType(owner_decl);
    auto owner_field =
        memberExpr(member(fieldDecl().bind("owner_field")),
                   hasObjectExpression(cxxThisExpr()), unless(is_owner_type));
    auto stmtInDtor = hasAncestor(cxxDestructorDecl());
    auto results = match(
        callExpr(
            anyOf(cxxMemberCallExpr(callee(cxxMethodDecl(hasName("Release"))),
                                    on(owner_field)),
                  callExpr(callee(functionDecl(hasName("SafeRelease"))),
                           hasArgument(0, owner_field))),
            stmtInDtor),
        ast_context);

    clang::SourceManager& source_manager = ast_context.getSourceManager();
    const clang::LangOptions& lang_opts = ast_context.getLangOpts();

    for (const auto& bound_nodes : results) {
      if (auto field_decl =
              bound_nodes.getNodeAs<clang::FieldDecl>("owner_field")) {
        auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();

        auto field_type_text =
            clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(
                                            field_type_loc.getSourceRange()),
                                        source_manager, lang_opts);
        std::string new_text = ("gpos::owner<" + field_type_text + ">").str();

        clang::tooling::Replacement annotation(
            source_manager,
            clang::CharSourceRange::getTokenRange(
                field_type_loc.getSourceRange()),
            new_text, lang_opts);
        std::string file_path = annotation.getFilePath().str();
        llvm::cantFail(replacements_[file_path].add(annotation));
      }
    }
  }
};

std::unique_ptr<clang::ASTConsumer> AnnotateAction::newASTConsumer() {
  return std::make_unique<AnnotateASTConsumer>(replacements_);
}

}  // namespace orca_tidy
