#include "Converter.h"
#include "AstHelpers.h"
#include "SwitchMatcher.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/FormatVariadic.h"

// NOLINTNEXTLINE: google-build-using-namespace
using namespace clang::ast_matchers;
namespace tooling = clang::tooling;

namespace orca_tidy {
static const constexpr llvm::StringRef kRefAnnotation = "gpos::Ref";

namespace {
class ConverterAstConsumer : public clang::ASTConsumer,
                             public AstHelperMixin<ConverterAstConsumer> {
 public:
  void Initialize(clang::ASTContext& context) override { context_ = &context; }
  clang::ASTContext& AstContext() const { return *context_; }

  ConverterAstConsumer(
      std::map<std::string, tooling::Replacements>& file_to_replaces)
      : file_to_replaces_(file_to_replaces) {}

  void HandleTranslationUnit(clang::ASTContext& context) override {
    ConvertCcacheTypedefs();
    ConvertRefArrayTypedefs();
    ConvertHashMapTypedefs();
    ConvertPointers();
    ConvertOwners();
    EraseAddRefs();
    ConvertOwnerToPointerImpCastToGet();
    ConvertCallCastFuncs();
  }

 private:
  void StripPointer(clang::TypeLoc type_loc) const;
  llvm::StringRef GetSourceTextOfTemplateArg(
      clang::TemplateSpecializationTypeLoc specialization_type_loc,
      unsigned int i) const;
  void OwnerToRef(clang::TypeLoc type_loc) const;
  void DotGet(const clang::Expr* e) const;
  void EraseStmt(const clang::Stmt* e) const;

  void ConvertCcacheTypedefs() const;
  void ConvertRefArrayTypedefs() const;
  void ConvertHashMapTypedefs() const;
  void ConvertPointers() const;
  void ConvertOwners() const;
  void EraseAddRefs() const;
  void ConvertOwnerToPointerImpCastToGet() const;
  void ConvertCallCastFuncs() const;

  std::map<std::string, tooling::Replacements>& file_to_replaces_;
  clang::ASTContext* context_;
};

void ConverterAstConsumer::ConvertPointers() const {
  for (const auto* field : NodesFromMatchAST<clang::FieldDecl>(
           fieldDecl(hasType(PointerType())).bind("pointer_field"),
           "pointer_field")) {
    StripPointer(field->getTypeSourceInfo()->getTypeLoc());
  }

  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           functionDecl(returns(anyOf(PointerType(), CastType()))).bind("f"),
           "f")) {
    StripPointer(f->getFunctionTypeLoc().getReturnLoc());
  }

  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(PointerType())).bind("v"), "v")) {
    StripPointer(v->getTypeSourceInfo()->getTypeLoc());
  }

  for (const auto* t : NodesFromMatchAST<clang::TypedefNameDecl>(
           typedefNameDecl(hasType(IsAnyFunctionType(Returns(PointerType()))))
               .bind("typedef_decl"),
           "typedef_decl")) {
    StripPointer(ExtractFunctionTypeLoc(t).getReturnLoc());
  }
}

void ConverterAstConsumer::ConvertOwners() const {
  for (const auto* field : NodesFromMatchAST<clang::FieldDecl>(
           fieldDecl(hasType(OwnerType())).bind("owner_field"),
           "owner_field")) {
    OwnerToRef(field->getTypeSourceInfo()->getTypeLoc());
  }

  for (const auto* f : NodesFromMatchAST<clang::FunctionDecl>(
           functionDecl(returns(OwnerType())).bind("f"), "f")) {
    OwnerToRef(f->getFunctionTypeLoc().getReturnLoc());
  }

  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(qualType(anyOf(OwnerType(), LeakedType()))))
               .bind("v"),
           "v")) {
    OwnerToRef(v->getTypeSourceInfo()->getTypeLoc());
  }

  for (const auto* e : NodesFromMatchAST<clang::Expr>(
           callExpr(ReleaseCallExpr(IgnoringParenCastFuncs(anyOf(
                        IgnoringStdMove(
                            declRefExpr(to(varDecl(hasType(OwnerType()))))),
                        FieldReferenceFor(fieldDecl(hasType(OwnerType())))))))
               .bind("e"),
           "e")) {
    EraseStmt(e);
  }

  for (const auto* t : NodesFromMatchAST<clang::TypedefNameDecl>(
           typedefNameDecl(hasType(IsAnyFunctionType(Returns(OwnerType()))))
               .bind("typedef_decl"),
           "typedef_decl")) {
    OwnerToRef(ExtractFunctionTypeLoc(t).getReturnLoc());
  }
}

void ConverterAstConsumer::EraseStmt(const clang::Stmt* e) const {
  tooling::Replacement r{
      SourceManager(),
      clang::CharSourceRange::getTokenRange(e->getSourceRange()), "",
      LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::StripPointer(clang::TypeLoc type_loc) const {
  auto range = clang::CharSourceRange::getTokenRange(type_loc.getSourceRange());

  auto template_specialization =
      IgnoringElaboratedQualified(type_loc)
          .getAs<clang::TemplateSpecializationTypeLoc>();
  auto inner_source_range =
      template_specialization.getArgLoc(0).getSourceRange();
  // Whoa, why? Because we otherwise lose cv-qualifiers of the template
  // argument. Don't trust me? There's a test!
  inner_source_range.setBegin(
      template_specialization.getLAngleLoc().getLocWithOffset(1));

  auto replacement_text = GetSourceText(inner_source_range);

  tooling::Replacement r{SourceManager(), range, replacement_text, LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::OwnerToRef(clang::TypeLoc type_loc) const {
  auto range = clang::CharSourceRange::getTokenRange(type_loc.getSourceRange());

  auto inner_source_range =
      GetPointeeLocOfFirstTemplateArg(IgnoringElaboratedQualified(type_loc))
          .getSourceRange();
  auto pointee_spelling = GetSourceText(inner_source_range);
  auto replacement_text = ("gpos::Ref<" + pointee_spelling + ">").str();

  tooling::Replacement r{SourceManager(), range, replacement_text, LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::ConvertCcacheTypedefs() const {
  auto specialization = classTemplateSpecializationDecl(
      hasAnyName("CCache", "CCacheAccessor"),
      hasTemplateArgument(0, refersToType(pointerType())));
  for (auto [tt, vv, auto_p] :
       NodesFromMatchAST<clang::TypedefNameDecl, clang::VarDecl,
                         clang::VarDecl>(
           decl(anyOf(
               typedefNameDecl(
                   hasType(qualType(hasDeclaration(specialization))))
                   .bind("typedef_decl"),
               varDecl(unless(isInstantiated()),
                       hasType(qualType(anyOf(hasDeclaration(specialization),
                                              pointsTo(specialization)))))
                   .bind("var"),
               varDecl(hasType(IgnoringElaborated(templateSpecializationType(
                           hasTemplateArgument(0, refersToType(hasDeclaration(
                                                      specialization)))))))
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

  for (const auto* ref : NodesFromMatchAST<clang::DeclRefExpr>(
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

llvm::StringRef orca_tidy::ConverterAstConsumer::GetSourceTextOfTemplateArg(
    clang::TemplateSpecializationTypeLoc specialization_type_loc,
    unsigned int i) const {
  return GetSourceText(specialization_type_loc.getArgLoc(i).getSourceRange());
}

void ConverterAstConsumer::ConvertRefArrayTypedefs() const {
  for (const auto* typedef_decl : NodesFromMatchAST<clang::TypedefNameDecl>(
           typedefNameDecl(hasType(qualType(hasDeclaration(RefArrayDecl()))))
               .bind("typedef_decl"),
           "typedef_decl")) {
    auto underlying_type_loc = typedef_decl->getTypeSourceInfo()->getTypeLoc();
    auto range = clang::CharSourceRange::getTokenRange(
        underlying_type_loc.getSourceRange());
    auto specialization_type_loc =
        underlying_type_loc
            .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
    auto elem_type_spelling =
        GetSourceTextOfTemplateArg(specialization_type_loc, 0);
    auto new_type_spelling =
        ("gpos::Vector<gpos::Ref<" + elem_type_spelling + ">>").str();
    tooling::Replacement r{SourceManager(), range, new_type_spelling,
                           LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }
}

void ConverterAstConsumer::ConvertHashMapTypedefs() const {
  auto ExtractTemplateArgs =
      [this](clang::TemplateSpecializationTypeLoc specialization_type_loc) {
        return std::tuple{
            GetSourceTextOfTemplateArg(specialization_type_loc, 0),
            GetSourceTextOfTemplateArg(specialization_type_loc, 1),
            GetSourceTextOfTemplateArg(specialization_type_loc, 2),
            GetSourceTextOfTemplateArg(specialization_type_loc, 3),
        };
      };
  for (auto [typedef_decl, hm] :
       NodesFromMatchAST<clang::TypedefNameDecl, clang::Decl>(
           typedefNameDecl(hasType(qualType(hasDeclaration(
                               anyOf(decl(HashMapRefKRefTDecl()).bind("hm"),
                                     HashMapIterRefKRefTDecl())))))
               .bind("typedef_decl"),
           "typedef_decl", "hm")) {
    auto underlying_type_loc = typedef_decl->getTypeSourceInfo()->getTypeLoc();
    auto specialization_type_loc =
        underlying_type_loc
            .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
    auto range = clang::CharSourceRange::getTokenRange(
        underlying_type_loc.getSourceRange());
    auto [k_spelling, t_spelling, hash_spelling, eq_spelling] =
        ExtractTemplateArgs(specialization_type_loc);
    const char* const
        fmtHM = R"C++(gpos::UnorderedMap<gpos::Ref<{0}>, gpos::Ref<{1}>,
                                         gpos::RefHash<{0}, {2}>,
                                         gpos::RefEq<{0}, {3}>>)C++";
    const char* const fmtHMI =
        R"C++(gpos::UnorderedMap<gpos::Ref<{0}>, gpos::Ref<{1}>,
                                 gpos::RefHash<{0}, {2}>,
                                 gpos::RefEq<{0}, {3}>>::LegacyIterator)C++";
    std::string new_type_spelling =
        llvm::formatv(hm ? fmtHM : fmtHMI, k_spelling, t_spelling,
                      hash_spelling, eq_spelling)
            .str();
    tooling::Replacement r{SourceManager(), range, new_type_spelling,
                           LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }
}

void ConverterAstConsumer::ConvertOwnerToPointerImpCastToGet() const {
  auto has_owner_type =
      anyOf(hasType(qualType(anyOf(OwnerType(), LeakedType()))),
            CallRefArraySubscript());
  auto return_owner_as_pointer = returnStmt(
      hasReturnValue(ignoringParenImpCasts(expr(has_owner_type).bind("owner"))),
      forFunction(returns(PointerType())));
  auto init_pointer_vars = declStmt(ForEachDeclaration(varDecl(
      hasType(PointerType()), hasInitializer(IgnoringParenCastFuncs(
                                  expr(has_owner_type).bind("owner"))))));
  auto assign_to_pointer_vars =
      AssignTo(expr(hasType(PointerType())),
               IgnoringParenCastFuncs(expr(has_owner_type).bind("owner")));
  auto pass_owner_arg_to_poiner_param =
      expr(IgnoringParenCastFuncs(invocation(ForEachArgumentWithParamType(
          expr(has_owner_type).bind("owner"), PointerType()))));
  for (const auto* owner : NodesFromMatchAST<clang::Expr>(
           stmt(anyOf(return_owner_as_pointer, init_pointer_vars,
                      assign_to_pointer_vars, pass_owner_arg_to_poiner_param)),
           "owner")) {
    DotGet(owner);
  }

  for (const auto* owner : NodesFromMatchAST<clang::Expr>(
           cxxCtorInitializer(
               forField(hasType(PointerType())),
               withInitializer(expr(has_owner_type).bind("owner"))),
           "owner")) {
    DotGet(owner);
  }
}

void ConverterAstConsumer::DotGet(const clang::Expr* e) const {
  auto range = clang::CharSourceRange::getTokenRange(e->getSourceRange());
  auto e_src_txt = GetSourceText(e->getSourceRange()).str();
  tooling::Replacement r{SourceManager(), range, e_src_txt + ".get()",
                         LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::ConvertCallCastFuncs() const {
  for (auto [call, enclosing_function] :
       NodesFromMatchAST<clang::CallExpr, clang::FunctionDecl>(
           callExpr(optionally(stmt(isInTemplateInstantiation(),
                                    forFunction(functionDecl().bind("f")))),
                    hasType(CastType()))
               .bind("call"),
           "call", "f")) {
    auto source_range = call->getCallee()->getSourceRange();
    auto range = clang::CharSourceRange::getTokenRange(source_range);

    auto PointeeSpellingFromDirectCallee =
        [this](const clang::FunctionDecl* direct_callee) {
          auto pointee_source_range =
              GetPointeeLocOfFirstTemplateArg(
                  direct_callee->getFunctionTypeLoc().getReturnLoc())
                  .getSourceRange();
          return GetSourceText(pointee_source_range);
        };

    auto ExprInTemplate = [this](
                              clang::SourceRange callee_source_range,
                              const clang::FunctionDecl* enclosing_function) {
      auto* func_in_primary_template =
          enclosing_function->getTemplateInstantiationPattern();
      auto exprs_in_primary_template = NodesFromMatchNode<clang::Expr>(
          traverse(clang::TK_IgnoreUnlessSpelledInSource,
                   stmt(forEachDescendant(
                       expr(HasSourceRange(callee_source_range)).bind("e")))),
          *func_in_primary_template->getBody(), "e");
      assert(exprs_in_primary_template.size() == 1);
      return exprs_in_primary_template.front();
    };

    auto PointeeSpellingFromNestedNameSpecifier =
        [](const clang::Expr* callee_in_template) {
          assert(callee_in_template->isTypeDependent());
          const auto* dre = llvm::dyn_cast<clang::DependentScopeDeclRefExpr>(
              callee_in_template);
          const auto* me = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(
              callee_in_template);
          assert(dre || me);
          auto* callee_qualifier =
              dre ? dre->getQualifier() : me->getQualifier();
          const auto* qualifier_as_type = callee_qualifier->getAsType();
          assert(qualifier_as_type);
          const auto* template_type_parm_type =
              llvm::dyn_cast<clang::TemplateTypeParmType>(qualifier_as_type);
          assert(template_type_parm_type);
          return template_type_parm_type->getDecl()->getName();
        };

    llvm::StringRef pointee_spelling;
    if (enclosing_function) {
      const clang::Expr* callee_in_template =
          ExprInTemplate(source_range, enclosing_function);
      if (const auto* direct_callee = llvm::cast_or_null<clang::FunctionDecl>(
              callee_in_template->getReferencedDeclOfCallee())) {
        pointee_spelling = PointeeSpellingFromDirectCallee(direct_callee);
      } else {
        pointee_spelling =
            PointeeSpellingFromNestedNameSpecifier(callee_in_template);
      }
    } else {
      const auto* direct_callee = call->getDirectCallee();
      pointee_spelling = PointeeSpellingFromDirectCallee(direct_callee);
    }
    auto replacement_text = ("gpos::dyn_cast<" + pointee_spelling + ">").str();
    tooling::Replacement r{SourceManager(), range, replacement_text,
                           LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }
}

void ConverterAstConsumer::EraseAddRefs() const {
  for (const auto* e : NodesFromMatchAST<clang::Expr>(
           callExpr(AddRefOn(expr())).bind("e"), "e")) {
    EraseStmt(e);
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
