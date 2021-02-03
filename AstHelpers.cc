#include "AstHelpers.h"
#include "clang/Lex/Lexer.h"

// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

namespace tooling = clang::tooling;

namespace orca_tidy {

TypeMatcher AnnotationType(NamedMatcher named_matcher) {
  return qualType(hasDeclaration(typeAliasTemplateDecl(named_matcher)));
}
TypeMatcher OwnerType() { return AnnotationType(hasName("::gpos::owner")); }
TypeMatcher PointerType() { return AnnotationType(hasName("::gpos::pointer")); }
TypeMatcher LeakedType() { return AnnotationType(hasName("::gpos::leaked")); }
TypeMatcher AnnotatedType() {
  return AnnotationType(
      hasAnyName("::gpos::pointer", "::gpos::owner", "::gpos::leaked"));
}

clang::TypeLoc IgnoringElaboratedQualified(clang::TypeLoc type_loc) {
  while (type_loc.getTypeLocClass() == clang::TypeLoc::Elaborated ||
         type_loc.getTypeLocClass() == clang::TypeLoc::Qualified) {
    type_loc = type_loc.getNextTypeLoc();
  }
  return type_loc;
}

void AnnotateSourceRange(
    clang::SourceRange source_range, llvm::StringRef annotation,
    const clang::ASTContext& ast_context,
    std::map<std::string, tooling::Replacements>& replacements) {
  auto range = clang::CharSourceRange::getTokenRange(source_range);
  const auto& source_manager = ast_context.getSourceManager();
  const auto& lang_opts = ast_context.getLangOpts();
  auto type_text =
      clang::Lexer::getSourceText(range, source_manager, lang_opts);

  std::string new_text = (annotation + "<" + type_text + ">").str();

  tooling::Replacement replacement(source_manager, range, new_text, lang_opts);
  std::string file_path = replacement.getFilePath().str();
  CantFail(replacements[file_path].add(replacement));
}

void CantFail(llvm::Error error) noexcept {
  if (!error) [[likely]]
    return;
  llvm::errs() << llvm::toString(std::move(error)) << '\n';
  std::terminate();
}
}  // namespace orca_tidy
