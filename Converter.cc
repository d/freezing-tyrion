#include "Converter.h"
#include "AstHelpers.h"
#include "SwitchMatcher.h"
#include "clang/Lex/Lexer.h"
#include "llvm/ADT/StringSwitch.h"
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
    ConvertHashMap();
    ConvertHashSetTypedefs();
    ConvertPointers();
    ConvertOwners();
    EraseAddRefs();
    ConvertOwnerToPointerImpCastToGet();
    ConvertAutoRef();
    ReplaceSortFunctors();
  }

 private:
  void StripPointer(clang::TypeLoc type_loc) const;
  void AddStar(const clang::VarDecl* v) const;
  llvm::StringRef GetSourceTextOfTemplateArg(
      clang::TemplateSpecializationTypeLoc specialization_type_loc,
      unsigned int i) const;
  void OwnerToRef(clang::TypeLoc type_loc) const;
  void DotGet(const clang::Expr* e) const;
  void EraseSourcRange(const clang::SourceRange source_range) const;
  void EraseStmt(const clang::Stmt* e) const;
  void EraseDecl(const clang::Decl* d) const;

  void ConvertCcacheTypedefs() const;
  void ConvertRefArrayTypedefs() const;
  void ConvertHashMap() const;
  void ConvertHashSetTypedefs() const;
  void ConvertPointers() const;
  void ConvertOwners() const;
  void EraseAddRefs() const;
  void ConvertOwnerToPointerImpCastToGet() const;
  void ConvertAutoRef() const;
  void ReplaceSortFunctors() const;

  std::map<std::string, tooling::Replacements>& file_to_replaces_;
  clang::ASTContext* context_;
};

static StatementMatcher VarSandwiching(const DeclarationMatcher& var1,
                                       const DeclarationMatcher& var2,
                                       const StatementMatcher& assignment) {
  return declStmt(hasSingleDecl(var1),
                  StmtIsImmediatelyBefore(declStmt(
                      has(var2), StmtIsImmediatelyBefore(assignment))));
}

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
    if (const auto* decl_stmt = GetParentAs<clang::DeclStmt>(*v, AstContext());
        decl_stmt && !decl_stmt->isSingleDecl()) {
      auto vars = llvm::map_range(decl_stmt->decls(), [](const clang::Decl* d) {
        return llvm::cast<clang::VarDecl>(d);
      });
      const auto* front = *vars.begin();
      StripPointer(front->getTypeSourceInfo()->getTypeLoc());
      for (const auto* var : llvm::drop_begin(vars)) {
        AddStar(var);
      }
    } else {
      StripPointer(v->getTypeSourceInfo()->getTypeLoc());
    }
  }

  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(pointsTo(PointerType()))).bind("v"), "v")) {
    StripPointer(v->getTypeSourceInfo()
                     ->getTypeLoc()
                     .getAs<clang::PointerTypeLoc>()
                     .getPointeeLoc());
  }

  for (const auto* t : NodesFromMatchAST<clang::TypedefNameDecl>(
           typedefNameDecl(hasType(IsAnyFunctionType(Returns(PointerType()))))
               .bind("typedef_decl"),
           "typedef_decl")) {
    StripPointer(ExtractFunctionProtoTypeLoc(t).getReturnLoc());
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

  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(pointsTo(OwnerType()))).bind("v"), "v")) {
    OwnerToRef(v->getTypeSourceInfo()
                   ->getTypeLoc()
                   .getAs<clang::PointerTypeLoc>()
                   .getPointeeLoc());
  }

  for (StatementMatcher
           ref_to_owner_var = declRefExpr(to(varDecl(hasType(OwnerType())))),
           deref_owner_star =
               Deref(declRefExpr(to(varDecl(hasType(pointsTo(OwnerType()))))));
       const auto *e : NodesFromMatchAST<clang::Expr>(
           callExpr(ReleaseCallExpr(IgnoringParenCastFuncs(anyOf(
                        IgnoringStdMove(ref_to_owner_var), deref_owner_star,
                        FieldReferenceFor(fieldDecl(hasType(OwnerType())))))))
               .bind("e"),
           "e")) {
    EraseStmt(e);
  }

  for (const auto* t : NodesFromMatchAST<clang::TypedefNameDecl>(
           typedefNameDecl(hasType(IsAnyFunctionType(Returns(OwnerType()))))
               .bind("typedef_decl"),
           "typedef_decl")) {
    OwnerToRef(ExtractFunctionProtoTypeLoc(t).getReturnLoc());
  }
}
void orca_tidy::ConverterAstConsumer::EraseSourcRange(
    const clang::SourceRange source_range) const {
  tooling::Replacement r{SourceManager(),
                         clang::CharSourceRange::getTokenRange(source_range),
                         "", LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::EraseStmt(const clang::Stmt* e) const {
  EraseSourcRange(e->getSourceRange());
}
void orca_tidy::ConverterAstConsumer::EraseDecl(const clang::Decl* d) const {
  EraseSourcRange(d->getSourceRange());
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

void ConverterAstConsumer::ConvertHashMap() const {
  auto ExtractTemplateArgs =
      [this](clang::TemplateSpecializationTypeLoc specialization_type_loc) {
        return std::tuple{
            GetSourceTextOfTemplateArg(specialization_type_loc, 0),
            GetSourceTextOfTemplateArg(specialization_type_loc, 1),
            GetSourceTextOfTemplateArg(specialization_type_loc, 2),
            GetSourceTextOfTemplateArg(specialization_type_loc, 3),
        };
      };
  for (auto [typedef_decl, v, hm] :
       NodesFromMatchAST<clang::TypedefNameDecl, clang::VarDecl, clang::Decl>(
           mapAnyOf(typedefNameDecl, varDecl)
               .with(unless(isInstantiated()),
                     hasType(qualType(hasDeclaration(
                         anyOf(decl(HashMapRefKRefTDecl()).bind("hm"),
                               HashMapIterRefKRefTDecl())))),
                     unless(hasDeclContext(
                         classTemplateDecl(hasName("gpos::CHashMapIter")))),
                     anyOf(typedefNameDecl().bind("typedef_decl"),
                           varDecl().bind("var"))),
           "typedef_decl", "var", "hm")) {
    clang::TypeLoc underlying_type_loc =
        (typedef_decl ? typedef_decl->getTypeSourceInfo()
                      : v->getTypeSourceInfo())
            ->getTypeLoc();
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

void orca_tidy::ConverterAstConsumer::ConvertHashSetTypedefs() const {
  for (auto [typedef_decl, hs] :
       NodesFromMatchAST<clang::TypedefNameDecl, clang::Decl>(
           typedefNameDecl(
               unless(isInstantiated()),
               hasType(qualType(hasDeclaration(anyOf(
                   decl(RefHashSetDecl()).bind("hs"), RefHashSetIterDecl())))),
               unless(hasDeclContext(
                   classTemplateDecl(hasName("gpos::CHashSetIter")))))
               .bind("typedef_decl"),
           "typedef_decl", "hs")) {
    auto underlying_type_loc = typedef_decl->getTypeSourceInfo()->getTypeLoc();
    auto range = clang::CharSourceRange::getTokenRange(
        underlying_type_loc.getSourceRange());
    auto specialization_type_loc =
        underlying_type_loc
            .getAsAdjusted<clang::TemplateSpecializationTypeLoc>();

    auto k_spelling = GetSourceTextOfTemplateArg(specialization_type_loc, 0),
         hash_spelling = GetSourceTextOfTemplateArg(specialization_type_loc, 1),
         eq_spelling = GetSourceTextOfTemplateArg(specialization_type_loc, 2);

    const char fmtHashSet[] =
        R"C++(gpos::UnorderedSet<gpos::Ref<{0}>, gpos::RefHash<{0}, {1}>,
                                 gpos::RefEq<{0}, {2}>>)C++";
    const char fmtHashSetIter[] =
        R"C++(gpos::UnorderedSet<gpos::Ref<{0}>, gpos::RefHash<{0}, {1}>,
                                 gpos::RefEq<{0}, {2}>>::LegacyIterator)C++";
    auto new_type_spelling =
        llvm::formatv(hs ? fmtHashSet : fmtHashSetIter, k_spelling,
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
  auto owner_expr = expr(has_owner_type).bind("owner");
  auto return_owner_as_pointer =
      returnStmt(hasReturnValue(ignoringParenImpCasts(owner_expr)),
                 forFunction(returns(PointerType())));
  auto init_pointer_vars = declStmt(ForEachDeclaration(
      varDecl(hasType(PointerType()),
              hasInitializer(IgnoringParenCastFuncs(owner_expr)))));
  auto assign_to_pointer_vars = AssignTo(expr(hasType(PointerType())),
                                         IgnoringParenCastFuncs(owner_expr));
  auto pass_owner_arg_to_pointer_param = expr(IgnoringParenCastFuncs(
      CallOrConstruct(Switch()
                          .Case(hasDeclaration(cxxConstructorDecl(
                                    ofClass(HashMapIterDecl()))),
                                hasArgument(0, owner_expr))
                          .Case(hasDeclaration(cxxConstructorDecl(
                                    ofClass(HashSetIterDecl()))),
                                hasArgument(0, owner_expr))
                          .Case(hasDeclaration(cxxMethodDecl(
                                    hasName("::gpdxl::CDXLUtils::Serialize"))),
                                hasArgument(1, owner_expr))
                          .Case(hasDeclaration(AddRefAppendMethod()),
                                ForEachArgumentWithParam(owner_expr, decl()))
                          .Default(ForEachArgumentWithParamType(
                              owner_expr, PointerType())))));
  for (const auto* owner : NodesFromMatchAST<clang::Expr>(
           stmt(anyOf(return_owner_as_pointer, init_pointer_vars,
                      assign_to_pointer_vars, pass_owner_arg_to_pointer_param)),
           "owner")) {
    DotGet(owner);
  }

  for (const auto* owner : NodesFromMatchAST<clang::Expr>(
           cxxCtorInitializer(forField(hasType(PointerType())),
                              withInitializer(owner_expr)),
           "owner")) {
    DotGet(owner);
  }
}

void ConverterAstConsumer::DotGet(const clang::Expr* e) const {
  auto range = clang::CharSourceRange::getTokenRange(e->getSourceRange());
  std::string replacement_text;
  if (const auto* uo = llvm::dyn_cast<clang::UnaryOperator>(e);
      uo && uo->getOpcode() == clang::UO_Deref) {
    replacement_text = llvm::formatv(
        "{0}->get()", GetSourceText(uo->getSubExpr()->getSourceRange()));
  } else {
    auto e_src_txt = GetSourceText(e->getSourceRange());
    replacement_text = llvm::formatv("{0}.get()", e_src_txt);
  }
  tooling::Replacement r{SourceManager(), range, replacement_text, LangOpts()};
  CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
}

void ConverterAstConsumer::EraseAddRefs() const {
  for (const auto* e : NodesFromMatchAST<clang::Expr>(
           callExpr(AddRefOn(expr())).bind("e"), "e")) {
    if (IsInMacro(e->getSourceRange())) continue;

    EraseStmt(e);
  }
}

void ConverterAstConsumer::ConvertAutoRef() const {
  for (const auto* v : NodesFromMatchAST<clang::VarDecl>(
           varDecl(hasType(AutoRefDecl()),
                   hasInitializer(cxxConstructExpr(
                       hasArgument(0, ignoringParenImpCasts(callExpr())))))
               .bind("v"),
           "v")) {
    auto type_loc = v->getTypeSourceInfo()->getTypeLoc();
    auto specialization_type_loc =
        type_loc.getAsAdjusted<clang::TemplateSpecializationTypeLoc>();
    auto elem_type_spelling =
        GetSourceTextOfTemplateArg(specialization_type_loc, 0);

    tooling::Replacement r{
        SourceManager(),
        clang::CharSourceRange::getTokenRange(type_loc.getSourceRange()),
        llvm::formatv("gpos::Ref<{0}>", elem_type_spelling).str(), LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }

  for (auto [auto_ref, assign, dre] :
       NodesFromMatchAST<clang::VarDecl, clang::Expr, clang::Expr>(
           VarSandwiching(
               varDecl(hasType(AutoRefDecl())).bind("auto_ref"),
               varDecl(hasType(OwnerType())).bind("v"),
               stmt(
                   AssignTo(
                       declRefExpr(to(equalsBoundNode("auto_ref"))).bind("dre"),
                       IgnoringParenCastFuncs(
                           declRefExpr(to(equalsBoundNode("v"))))))
                   .bind("assign")),
           "auto_ref", "assign", "dre")) {
    const auto* compound_stmt = GetParentAs<clang::Stmt>(*assign, AstContext());
    assert(compound_stmt);

    if (Match(stmt(hasDescendant(declRefExpr(unless(equalsNode(dre)),
                                             to(equalsNode(auto_ref))))),
              *compound_stmt))
      continue;

    EraseDecl(auto_ref);
    EraseStmt(assign);
  }
}

void ConverterAstConsumer::ReplaceSortFunctors() const {
  auto known_sort_cmp_fn = declRefExpr(
      to(functionDecl(hasAnyName("IDatumCmp", "ICmpPrjElemsArr", "CPointCmp",
                                 "StatsPredSortCmpFunc"))
             .bind("f")));
  for (auto [f, arg] : NodesFromMatchAST<clang::FunctionDecl, clang::Expr>(
           cxxMemberCallExpr(
               callee(cxxMethodDecl(hasName("Sort"), ofClass(RefArrayDecl()))),
               argumentCountIs(1),
               hasArgument(0, ignoringParenImpCasts(
                                  expr(Switch()
                                           .Case(unaryOperator(),
                                                 AddrOf(known_sort_cmp_fn))
                                           .Default(known_sort_cmp_fn))
                                      .bind("arg")))),
           "f", "arg")) {
    auto name = f->getName();
    std::string replacement_text =
        llvm::StringSwitch<std::string>(name)
            .Case("IDatumCmp", "gpopt::DatumLess{}")
            .Case("ICmpPrjElemsArr", "gpopt::ProjectElementArrayLess{}")
            .Case("CPointCmp", "gpnaucrates::PointLess{}")
            .Case("StatsPredSortCmpFunc", "gpnaucrates::StatsPredColIdLess{}");

    auto range = clang::CharSourceRange::getTokenRange(arg->getSourceRange());
    tooling::Replacement r{SourceManager(), range, replacement_text,
                           LangOpts()};
    CantFail(file_to_replaces_[r.getFilePath().str()].add(r));
  }
}
void orca_tidy::ConverterAstConsumer::AddStar(const clang::VarDecl* v) const {
  auto id_range = clang::SourceRange{v->getLocation(), v->getEndLoc()};
  auto range = clang::CharSourceRange::getTokenRange(id_range);
  tooling::Replacement r{SourceManager(), range,
                         llvm::formatv("*{0}", GetSourceText(id_range)).str(),
                         LangOpts()};
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
