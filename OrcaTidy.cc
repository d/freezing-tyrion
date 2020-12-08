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
    auto field_reference_for = [](auto field_matcher,
                                  auto has_excluded_annotation) {
      return memberExpr(member(field_matcher),
                        hasObjectExpression(cxxThisExpr()),
                        unless(has_excluded_annotation));
    };
    auto owner_field =
        field_reference_for(fieldDecl().bind("owner_field"), is_owner_type);
    auto stmtInDtor = hasAncestor(cxxDestructorDecl());
    auto releaseCallExpr = [](auto reference_to_field) {
      return anyOf(cxxMemberCallExpr(callee(cxxMethodDecl(hasName("Release"))),
                                     on(reference_to_field)),
                   callExpr(callee(functionDecl(hasName("SafeRelease"))),
                            hasArgument(0, reference_to_field)));
    };
    auto results =
        match(callExpr(releaseCallExpr(owner_field), stmtInDtor), ast_context);

    clang::SourceManager& source_manager = ast_context.getSourceManager();
    const clang::LangOptions& lang_opts = ast_context.getLangOpts();

    for (const auto& bound_nodes : results) {
      if (auto field_decl =
              bound_nodes.getNodeAs<clang::FieldDecl>("owner_field")) {
        annotateField(field_decl, "gpos::owner", source_manager, lang_opts);
      }
    }

    auto ref_count_record_decl = cxxRecordDecl(
        isSameOrDerivedFrom(cxxRecordDecl(hasMethod(hasName("Release")))));

    auto is_pointer_type =
        hasType(typeAliasTemplateDecl(hasName("::gpos::pointer")));

    results = match(fieldDecl(unless(is_pointer_type),
                              hasType(pointsTo(ref_count_record_decl)))
                        .bind("field"),
                    ast_context);

    for (const auto& bound_nodes : results) {
      auto field_decl = bound_nodes.getNodeAs<clang::FieldDecl>("field");
      if (auto record = llvm::dyn_cast<clang::CXXRecordDecl>(
              field_decl->getDeclContext());
          record && record->getDestructor()->isDefined()) {
        auto dtor = record->getDestructor();

        auto reference_to_field =
            field_reference_for(equalsNode(field_decl), is_pointer_type);
        if (!match(decl(hasDescendant(
                       callExpr(releaseCallExpr(reference_to_field)))),
                   *dtor, ast_context)
                 .empty())
          continue;
        annotateField(field_decl, "gpos::pointer", source_manager, lang_opts);
      };
    }
  }

 private:
  void annotateField(const clang::FieldDecl* field_decl,
                     const std::string& annotation,
                     const clang::SourceManager& source_manager,
                     const clang::LangOptions& lang_opts) {
    auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();

    auto field_type_text = clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(field_type_loc.getSourceRange()),
        source_manager, lang_opts);
    std::string new_text = (annotation + "<" + field_type_text + ">").str();

    clang::tooling::Replacement annotation_rep(
        source_manager,
        clang::CharSourceRange::getTokenRange(field_type_loc.getSourceRange()),
        new_text, lang_opts);
    std::string file_path = annotation_rep.getFilePath().str();
    llvm::cantFail(replacements_[file_path].add(annotation_rep));
  }
};

std::unique_ptr<clang::ASTConsumer> AnnotateAction::newASTConsumer() {
  return std::make_unique<AnnotateASTConsumer>(replacements_);
}

}  // namespace orca_tidy
