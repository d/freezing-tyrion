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
      auto release = cxxMemberCallExpr(
          callee(cxxMethodDecl(hasName("Release"))), on(reference_to_field));
      auto safe_release = callExpr(callee(functionDecl(hasName("SafeRelease"))),
                                   hasArgument(0, reference_to_field));
      return callExpr(anyOf(release, safe_release));
    };
    auto results = match(releaseCallExpr(owner_field), ast_context);

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
                              hasType(pointsTo(hasCanonicalType(
                                  hasDeclaration(ref_count_record_decl)))))
                        .bind("field"),
                    ast_context);

    for (const auto& bound_nodes : results) {
      auto field_decl = bound_nodes.getNodeAs<clang::FieldDecl>("field");
      auto record =
          llvm::cast<clang::CXXRecordDecl>(field_decl->getDeclContext());
      if (!record) continue;
      auto dtor = record->getDestructor();
      // destructor not visible in this translation unit, leave unannotated
      if (dtor && !dtor->isDefined() && !dtor->isDefaulted()) continue;

      if (dtor) {
        auto reference_to_field =
            field_reference_for(equalsNode(field_decl), is_pointer_type);
        if (!match(decl(hasDescendant(releaseCallExpr(reference_to_field))),
                   *dtor, ast_context)
                 .empty())
          continue;

        // Here's the tricky part, we don't release this field in the
        // destructor, but there might still be a release in another method. Not
        // every method is necessarily visible in this translation unit. (Like
        // when the destructor is inline in the header).

        // We try a best-effort search. If we still don't see releases, we
        // proceed to recognize it as a pointer / observer.

        // Theoretically it's not hard to imagine adversarial code breaking
        // this. In those cases we'll need some fancy schmancy cross-TU trick.
        // But we seem to be able to get away with this heuristic in ORCA.
        if (!match(releaseCallExpr(reference_to_field), ast_context).empty())
          continue;
      }

      annotateField(field_decl, "gpos::pointer", source_manager, lang_opts);
    }

    for (const auto& bound_nodes :
         match(releaseCallExpr(declRefExpr(
                   to(parmVarDecl(unless(is_owner_type)).bind("owner_parm")))),
               ast_context)) {
      auto owner_parm = bound_nodes.getNodeAs<clang::ParmVarDecl>("owner_parm");
      auto function_scope_index = owner_parm->getFunctionScopeIndex();

      for (auto function = llvm::cast<clang::FunctionDecl>(
               owner_parm->getParentFunctionOrMethod());
           function; function = function->getPreviousDecl()) {
        auto parm = function->getParamDecl(function_scope_index);
        auto source_range =
            parm->getTypeSourceInfo()->getTypeLoc().getSourceRange();

        auto type_text = clang::Lexer::getSourceText(
            clang::CharSourceRange::getTokenRange(source_range), source_manager,
            lang_opts);

        std::string new_text = ("gpos::owner<" + type_text + ">").str();

        clang::tooling::Replacement replacement(
            source_manager, clang::CharSourceRange::getTokenRange(source_range),
            new_text, lang_opts);
        std::string file_path = replacement.getFilePath().str();
        llvm::cantFail(replacements_[file_path].add(replacement));
      }
    }
  }

 private:
  void annotateField(const clang::FieldDecl* field_decl,
                     llvm::StringRef annotation,
                     const clang::SourceManager& source_manager,
                     const clang::LangOptions& lang_opts) {
    auto field_type_loc = field_decl->getTypeSourceInfo()->getTypeLoc();
    clang::SourceRange type_range = field_type_loc.getSourceRange();
    auto field_qual_type = field_decl->getType();
    auto pointee_type = field_qual_type->getPointeeType();

    const char* opt_mutable = field_decl->isMutable() ? "mutable " : "";
    std::string pointee_cv;
    auto pointee_local_qualifiers = pointee_type.getLocalQualifiers();
    if (pointee_local_qualifiers.hasConst()) {
      pointee_cv = "const ";
    }
    if (pointee_local_qualifiers.hasVolatile()) {
      pointee_cv += "volatile ";
    }
    auto field_type_text = clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(type_range), source_manager,
        lang_opts);
    std::string new_text =
        (opt_mutable + annotation + "<" + pointee_cv + field_type_text + ">")
            .str();

    // HACK: notice that the replacement range isn't just the type but it also
    // extends to the beginning of the declarator. This is so that we cover the
    // cases of "const mutable T*" or "mutable const volatile T*"
    clang::tooling::Replacement annotation_rep(
        source_manager,
        clang::CharSourceRange::getTokenRange(field_decl->getBeginLoc(),
                                              field_type_loc.getEndLoc()),
        new_text, lang_opts);
    std::string file_path = annotation_rep.getFilePath().str();
    llvm::cantFail(replacements_[file_path].add(annotation_rep));
  }
};

std::unique_ptr<clang::ASTConsumer> AnnotateAction::newASTConsumer() {
  return std::make_unique<AnnotateASTConsumer>(replacements_);
}

}  // namespace orca_tidy
