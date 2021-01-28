#include "Converter.h"
#include "AstHelpers.h"
#include "clang/Lex/Lexer.h"

// NOLINTNEXTLINE: google-build-using-namespace
using namespace clang::ast_matchers;
namespace tooling = clang::tooling;

namespace orca_tidy {
static void CantFail(llvm::Error error) noexcept {
  if (!error) [[likely]]
    return;
  llvm::errs() << llvm::toString(std::move(error)) << '\n';
  std::terminate();
}

namespace {
auto IgnoringElaboratedQualified(clang::TypeLoc type_loc) {
  while (type_loc.getTypeLocClass() == clang::TypeLoc::Elaborated ||
         type_loc.getTypeLocClass() == clang::TypeLoc::Qualified) {
    type_loc = type_loc.getNextTypeLoc();
  }
  return type_loc;
}

class ConverterAstConsumer : public clang::ASTConsumer,
                             public NodesFromMatchBase<ConverterAstConsumer> {
 public:
  void Initialize(clang::ASTContext& context) override { context_ = &context; }
  clang::ASTContext& AstContext() const { return *context_; }

  ConverterAstConsumer(
      std::map<std::string, tooling::Replacements>& file_to_replaces)
      : file_to_replaces_(file_to_replaces) {}

  void HandleTranslationUnit(clang::ASTContext& context) override {
    ConvertPointerFields();
    ConvertOwnerFields();
  }

 private:
  clang::SourceManager& SourceManager() const {
    return context_->getSourceManager();
  }
  const clang::LangOptions& LangOpts() const { return context_->getLangOpts(); }

  void StripPointer(const clang::FieldDecl* field) const;
  void OwnerToRef(const clang::FieldDecl* field) const;

  void ConvertPointerFields() const;
  void ConvertOwnerFields() const;

  std::map<std::string, tooling::Replacements>& file_to_replaces_;
  clang::ASTContext* context_;
};

void orca_tidy::ConverterAstConsumer::ConvertPointerFields() const {
  for (const auto* field : NodesFromMatch<clang::FieldDecl>(
           fieldDecl(hasType(PointerType())).bind("pointer_field"),
           "pointer_field")) {
    StripPointer(field);
  }
}

void orca_tidy::ConverterAstConsumer::ConvertOwnerFields() const {
  for (const auto* field : NodesFromMatch<clang::FieldDecl>(
           fieldDecl(hasType(OwnerType())).bind("owner_field"),
           "owner_field")) {
    OwnerToRef(field);
  }
}

void orca_tidy::ConverterAstConsumer::StripPointer(
    const clang::FieldDecl* field) const {
  auto outer_loc = field->getTypeSourceInfo()->getTypeLoc();
  auto range =
      clang::CharSourceRange::getTokenRange(outer_loc.getSourceRange());

  auto template_specialization =
      IgnoringElaboratedQualified(outer_loc)
          .getAs<clang::TemplateSpecializationTypeLoc>();
  auto inner_source_range =
      template_specialization.getArgLoc(0).getSourceRange();
  // Whoa, why? Because we otherwise lose cv-qualifiers of the template
  // argument. Don't trust me? There's a test!
  inner_source_range.setBegin(
      template_specialization.getLAngleLoc().getLocWithOffset(1));

  auto inner_token_range =
      clang::CharSourceRange::getTokenRange(inner_source_range);
  auto replacement_text = clang::Lexer::getSourceText(
      inner_token_range, SourceManager(), LangOpts());

  tooling::Replacement r{SourceManager(), range, replacement_text, LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void orca_tidy::ConverterAstConsumer::OwnerToRef(
    const clang::FieldDecl* field) const {
  auto outer_loc = field->getTypeSourceInfo()->getTypeLoc();
  auto range =
      clang::CharSourceRange::getTokenRange(outer_loc.getSourceRange());

  auto template_specialization =
      IgnoringElaboratedQualified(outer_loc)
          .getAs<clang::TemplateSpecializationTypeLoc>();
  auto inner_source_range = template_specialization.getArgLoc(0)
                                .getTypeSourceInfo()
                                ->getTypeLoc()
                                .getAs<clang::PointerTypeLoc>()
                                .getPointeeLoc()
                                .getSourceRange();
  auto inner_token_range =
      clang::CharSourceRange::getTokenRange(inner_source_range);
  auto pointee_spelling = clang::Lexer::getSourceText(
      inner_token_range, SourceManager(), LangOpts());
  auto replacement_text = ("Ref<" + pointee_spelling + ">").str();

  tooling::Replacement r{SourceManager(), range, replacement_text, LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}
}  // namespace

Converter::Converter(
    std::map<std::string, tooling::Replacements>& file_to_replaces)
    : file_to_replaces_(file_to_replaces) {}

std::unique_ptr<clang::ASTConsumer> Converter::newASTConsumer() {
  return std::make_unique<ConverterAstConsumer>(file_to_replaces_);
}
}  // namespace orca_tidy
