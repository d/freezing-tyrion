#include "Converter.h"
#include "AstHelpers.h"
#include "clang/Lex/Lexer.h"

// NOLINTNEXTLINE: google-build-using-namespace
using namespace clang::ast_matchers;
namespace tooling = clang::tooling;

namespace orca_tidy {
static const constexpr llvm::StringRef kRefAnnotation = "gpos::Ref";

namespace {
class ConverterAstConsumer : public clang::ASTConsumer,
                             public NodesFromMatchBase<ConverterAstConsumer> {
 public:
  void Initialize(clang::ASTContext& context) override { context_ = &context; }
  clang::ASTContext& AstContext() const { return *context_; }

  ConverterAstConsumer(
      std::map<std::string, tooling::Replacements>& file_to_replaces)
      : file_to_replaces_(file_to_replaces) {}

  void ConvertCcacheTypedefs() const;
  void ConvertCcacheAccessorInsertOrValOrNext() const;

  void HandleTranslationUnit(clang::ASTContext& context) override {
    ConvertCcacheTypedefs();
    ConvertPointerFields();
    ConvertOwnerFields();
    ConvertCcacheAccessorInsertOrValOrNext();
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

void orca_tidy::ConverterAstConsumer::ConvertCcacheTypedefs() const {
  auto specialization = classTemplateSpecializationDecl(
      hasAnyName("CCache", "CCacheAccessor"),
      hasTemplateArgument(0, refersToType(pointerType())));
  for (auto [tt, vv, auto_p] :
       NodesFromMatch<clang::TypedefNameDecl, clang::VarDecl, clang::VarDecl>(
           decl(anyOf(
               typedefNameDecl(
                   hasType(qualType(hasDeclaration(specialization))))
                   .bind("typedef_decl"),
               varDecl(unless(isInstantiated()),
                       hasType(qualType(anyOf(hasDeclaration(specialization),
                                              pointsTo(specialization)))))
                   .bind("var"),
               varDecl(
                   hasType(classTemplateSpecializationDecl(hasTemplateArgument(
                       0, refersToType(hasDeclaration(specialization))))))
                   .bind("auto_p"))),
           "typedef_decl", "var", "auto_p")) {
    // We cannot capture structured bindings in lambdas, see
    // https://wg21.cmeerw.net/cwg/issue2308 and https://wg21.link/P0588R1
    const auto* t = tt;
    const auto* v = vv;
    const auto* ap = auto_p;
    auto specialization_loc = [=]() {
      if (t) {
        return t->getTypeSourceInfo()
            ->getTypeLoc()
            .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
      }
      if (ap) {
        auto auto_p_instantiation_loc =
            ap->getTypeSourceInfo()
                ->getTypeLoc()
                .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
        return auto_p_instantiation_loc.getArgLoc(0)
            .getTypeSourceInfo()
            ->getTypeLoc()
            .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
      }
      auto loc = v->getTypeSourceInfo()->getTypeLoc();
      if (auto pointer_loc = loc.getAsAdjusted<clang::PointerTypeLoc>();
          pointer_loc)
        loc = pointer_loc.getPointeeLoc();
      return loc.getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
    }();
    auto arg_loc = specialization_loc.getArgLoc(0)
                       .getTypeSourceInfo()
                       ->getTypeLoc()
                       .getAsAdjusted<clang::PointerTypeLoc>();
    AnnotateSourceRange(arg_loc.getSourceRange(),
                        arg_loc.getPointeeLoc().getSourceRange(),
                        kRefAnnotation, AstContext(), file_to_replaces_);
  }

  for (const auto* ref : NodesFromMatch<clang::DeclRefExpr>(
           callExpr(callee(expr(ignoringParenImpCasts(
               declRefExpr(to(functionDecl(isTemplateInstantiation(),
                                           returns(pointsTo(specialization)))))
                   .bind("ref"))))),
           "ref")) {
    if (ref->getNumTemplateArgs() != 2) continue;
    auto arg = ref->template_arguments()[0];
    auto arg_loc = arg.getTypeSourceInfo()
                       ->getTypeLoc()
                       .getAsAdjusted<clang::PointerTypeLoc>();
    AnnotateSourceRange(arg_loc.getSourceRange(),
                        arg_loc.getPointeeLoc().getSourceRange(),
                        kRefAnnotation, AstContext(), file_to_replaces_);
  }
}

void orca_tidy::ConverterAstConsumer::ConvertCcacheAccessorInsertOrValOrNext()
    const {
  for (const auto* v : NodesFromMatch<clang::VarDecl>(
           VarInitializedOrAssigned(
               varDecl().bind("var"),
               ignoringParenCasts(CallCcacheAccessorMethodsReturningOwner())),
           "var")) {
    auto pointer_loc = v->getTypeSourceInfo()
                           ->getTypeLoc()
                           .getAsAdjusted<clang::PointerTypeLoc>();
    AnnotateSourceRange(pointer_loc.getSourceRange(),
                        pointer_loc.getPointeeLoc().getSourceRange(),
                        kRefAnnotation, AstContext(), file_to_replaces_);
  }
}
}  // namespace

Converter::Converter(
    std::map<std::string, tooling::Replacements>& file_to_replaces)
    : file_to_replaces_(file_to_replaces) {}

std::unique_ptr<clang::ASTConsumer> Converter::newASTConsumer() {
  return std::make_unique<ConverterAstConsumer>(file_to_replaces_);
}
}  // namespace orca_tidy
