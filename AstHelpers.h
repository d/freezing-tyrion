#ifndef ORCATIDY__ASTHELPERS_H_
#define ORCATIDY__ASTHELPERS_H_
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace orca_tidy {

static const constexpr llvm::StringRef kOwnerAnnotation = "gpos::owner";
static const constexpr llvm::StringRef kPointerAnnotation = "gpos::pointer";
static const constexpr llvm::StringRef kCastAnnotation = "gpos::cast_func";

using ExpressionMatcher = decltype(clang::ast_matchers::nullPointerConstant());
using FunctionMatcher = decltype(clang::ast_matchers::isDefaulted());
using CXXMemberCallMatcher =
    decltype(clang::ast_matchers::on(std::declval<ExpressionMatcher>()));
using VarMatcher = decltype(clang::ast_matchers::hasLocalStorage());
using ParamMatcher = decltype(clang::ast_matchers::hasDefaultArgument());
using clang::ast_matchers::DeclarationMatcher;
using clang::ast_matchers::StatementMatcher;
using clang::ast_matchers::TemplateArgumentMatcher;
using clang::ast_matchers::TypeMatcher;

using NamedMatcher = decltype(clang::ast_matchers::hasName(""));
static TypeMatcher AnnotationType(NamedMatcher named_matcher);

__attribute__((const)) TypeMatcher OwnerType();

__attribute__((const)) TypeMatcher PointerType();

__attribute__((const)) TypeMatcher LeakedType();

__attribute__((const)) TypeMatcher CastType();

__attribute__((const)) TypeMatcher AnnotatedType();

__attribute__((const)) TypeMatcher RefCountPointerType();
__attribute__((const)) TypeMatcher RefCountPointerPointerType();

__attribute__((const)) StatementMatcher
CallCcacheAccessorMethodsReturningOwner();

__attribute__((const)) StatementMatcher CallCDynPtrArrSubscriptOn(
    const ExpressionMatcher& expr);
__attribute__((const)) StatementMatcher CallRefArraySubscript();

StatementMatcher Deref(const ExpressionMatcher& expr);
StatementMatcher AddrOf(const ExpressionMatcher& expr);

__attribute__((const)) DeclarationMatcher SingleDecl();

StatementMatcher AssignTo(const ExpressionMatcher& lhs);

StatementMatcher AssignTo(const ExpressionMatcher& lhs,
                          const ExpressionMatcher& rhs);

// the first-match equivalent of findAll
template <class Matcher>
auto SelfOrHasDescendant(const Matcher& matcher) {
  // NOLINTNEXTLINE(google-build-using-namespace)
  using namespace clang::ast_matchers;

  return anyOf(matcher, hasDescendant(matcher));
}

AST_MATCHER(clang::CallExpr, IsCallToStdMove) { return Node.isCallToStdMove(); }

AST_MATCHER(clang::CXXMethodDecl, IsStatic) { return Node.isStatic(); }

using DeclSet = llvm::DenseSet<const clang::Decl*>;
AST_MATCHER_P(clang::Decl, IsInSet, DeclSet, nodes) {
  return nodes.contains(&Node);
}

using StmtOrCXXCtorInitializer =
    llvm::PointerUnion<const clang::Stmt*, const clang::CXXCtorInitializer*>;
using LastUseStmts =
    llvm::SmallVector<std::tuple<StmtOrCXXCtorInitializer, DeclSet>>;

LastUseStmts LastUseStatementsOfFunc(const clang::FunctionDecl* c);

clang::QualType StripElaborated(clang::QualType qual_type);
TypeMatcher IgnoringElaborated(const TypeMatcher& type_matcher);
clang::QualType StripAnnotation(clang::QualType qual_type);
TypeMatcher IgnoringAnnotation(const TypeMatcher& inner_matcher);

bool IsCastFunc(const clang::Decl* decl);
const clang::Expr* IgnoreCastFuncs(const clang::Expr* expr);
AST_MATCHER_P(clang::Expr, IgnoringCastFuncs, ExpressionMatcher,
              inner_matcher) {
  return inner_matcher.matches(*IgnoreCastFuncs(&Node), Finder, Builder);
}

const clang::Expr* IgnoreParenCastFuncs(const clang::Expr* expr);
const clang::Expr* IgnoreStdMove(const clang::Expr* expr);

AST_MATCHER_P(clang::Expr, IgnoringParenCastFuncs, ExpressionMatcher,
              inner_matcher) {
  return inner_matcher.matches(*IgnoreParenCastFuncs(&Node), Finder, Builder);
}

AST_MATCHER_P(clang::Expr, IgnoringStdMove, ExpressionMatcher, inner_matcher) {
  return inner_matcher.matches(*IgnoreStdMove(&Node), Finder, Builder);
}

inline auto ForEachArgumentWithParam(ExpressionMatcher arg_matcher,
                                     ParamMatcher param_matcher) {
  return clang::ast_matchers::forEachArgumentWithParam(
      IgnoringParenCastFuncs(arg_matcher), param_matcher);
}

inline auto ForEachArgumentWithParamType(ExpressionMatcher arg_matcher,
                                         TypeMatcher param_matcher) {
  return clang::ast_matchers::forEachArgumentWithParamType(
      IgnoringParenCastFuncs(arg_matcher), param_matcher);
}

/// Whenever we think of using \c forEachArgumentWithParam or
/// \c forEachArgumentWithParamType with \c callExpr, we probably should also
/// consider using them with \c cxxConstructExpr . That's what these two
/// matchers, \c NonTemplateCallOrConstruct, and \c CallOrConstruct are for:
/// they will match either a \c CallExpr or a \c CXXConstructExpr with all your
/// passed in matchers.
/// In addition, we should consider the non-template version when we intend to
/// infer the parameter annotations for the callee, because function templates
/// (or methods of class templates) can exhibit different owning behavior
/// depending on the instantiation (c.f. \c CDynamicPtrArrary::Append)
template <class... Matchers>
static StatementMatcher CallOrConstruct(Matchers... matchers) {
  // NOLINTNEXTLINE(google-build-using-namespace)
  using namespace clang::ast_matchers;

  return expr(IgnoringParenCastFuncs(invocation(matchers...)));
}

template <class... Matchers>
static auto NonTemplateCallOrConstruct(Matchers... matchers) {
  // NOLINTNEXTLINE(google-build-using-namespace)
  using namespace clang::ast_matchers;

  return CallOrConstruct(
      unless(hasDeclaration(functionDecl(isTemplateInstantiation()))),
      matchers...);
}

bool IsUniqRefToDeclInStmt(const clang::Expr* e, const clang::Decl* d,
                           const clang::Stmt* s);

AST_MATCHER_P(clang::FunctionType, Returns, TypeMatcher, type_matcher) {
  return type_matcher.matches(Node.getReturnType(), Finder, Builder);
}

using FunctionTypeMatcher = decltype(Returns(std::declval<TypeMatcher>()));

AST_MATCHER_P(clang::Type, IsAnyFunctionType, FunctionTypeMatcher,
              function_type_matcher) {
  const auto* f = Node.getAsAdjusted<clang::FunctionType>();
  if (f) {
  } else if (const auto* pf = Node.getAsAdjusted<clang::PointerType>();
             pf && pf->isFunctionPointerType()) {
    f = pf->getPointeeType()->getAsAdjusted<clang::FunctionType>();
  } else if (const auto* pmf = Node.getAsAdjusted<clang::MemberPointerType>();
             pmf && pmf->isMemberFunctionPointer()) {
    f = pmf->getPointeeType()->getAsAdjusted<clang::FunctionType>();
  } else {
    return false;
  }
  return function_type_matcher.matches(*f, Finder, Builder);
}

clang::FunctionTypeLoc ExtractFunctionTypeLoc(
    const clang::TypedefNameDecl* typedef_name_decl);

template <class Range>
auto MakeVector(Range&& range) {
  using value_type = typename decltype(range.begin())::value_type;
  llvm::SmallVector<value_type> nodes(range.begin(), range.end());
  return nodes;
}

template <class... Nodes, class... IDs>
auto GetNode(const clang::ast_matchers::BoundNodes& bound_nodes, IDs... ids) {
  if constexpr (sizeof...(ids) == 1)
    return bound_nodes.template getNodeAs<Nodes...>(ids...);
  else
    return std::tuple{bound_nodes.template getNodeAs<Nodes>(ids)...};
}

AST_MATCHER_P(clang::DeclStmt, ForEachDeclaration, DeclarationMatcher,
              inner_matcher) {
  std::remove_pointer_t<decltype(Builder)> result;
  bool matched = false;
  for (const auto* d : Node.decls()) {
    auto matches = *Builder;
    if (inner_matcher.matches(*d, Finder, &matches)) {
      result.addMatch(matches);
      matched = true;
    }
  }
  *Builder = std::move(result);
  return matched;
}

template <class Derived>
class AstHelperMixin {
  clang::ASTContext& GetAstContext() const {
    return static_cast<const Derived*>(this)->AstContext();
  }

 public:
  clang::SourceManager& SourceManager() const {
    return GetAstContext().getSourceManager();
  }

  const clang::LangOptions& LangOpts() const {
    return GetAstContext().getLangOpts();
  }

  /// Convenience interface to go through the results of an AST match by
  /// directly iterating over node. For example, instead of saying
  ///
  /// \code
  /// for (const auto& bound_nodes : match(varDecl().bind("v"), ast_context)) {
  ///   const auto* v = bound_nodes.getNodeAs<VarDecl>("v");
  ///   ...
  /// }
  /// \endcode We get to say
  ///
  /// \code
  /// for (const auto* v = NodesFromMatchAST<VarDecl>(varDecl().bind("v"))) {
  /// ...
  /// }
  /// \endcode
  ///
  /// In addition, when you pass in two or more bindings, the returned sequence
  /// can be iterated through structured binding:
  ///
  /// \code
  /// for (auto [var, arg]: NodesFromMatchAST<VarDecl, Expr>(callExpr(
  ///          hasAnyArgument(declRefExpr(to(varDecl().bind("var"))).bind("arg")))
  ///          )) {
  ///   ...
  /// }
  /// \endcode
  ///
  /// Caveat: the returned sequence does NOT own the clang::BoundNodes objects
  /// returned from the original call to \code match() \endcode . Because most
  /// nodes (\c Decl, \c Stmt, \c Type) are pointer-stable and stored in the
  /// AST, this is practically not a problem. Notable exceptions are: \c
  /// QualType, \c NestedNameSpecifierLoc, \c TypeLoc, \c TemplateArgument and
  /// \c TemplateArgumentLoc. These are embedded as values (and stored as values
  /// in a \c BoundNodes object) and passed around as values. When you want to
  /// bind to one of those types, use the more ceremonious \code match()
  /// \endcode API instead.
  ///
  /// One interesting workaround for binding to a \c QualType is to bind to a
  /// \c Type instead, if you know your matcher doesn't examine the
  /// cv-qualifiers: e.g. Instead of
  ///
  /// \code
  /// functionDecl(returns(qualType.bind("t")))
  /// \endcode
  ///
  /// you can use
  ///
  /// \code
  /// functionDecl(returns(type.bind("t")))
  /// \endcode
  template <class... Nodes, class Matcher, class... Ids>
  auto NodesFromMatchAST(Matcher matcher, Ids... ids) const {
    auto matches = clang::ast_matchers::match(matcher, GetAstContext());
    auto nodes = MakeVector(llvm::map_range(
        matches, [ids...](const clang::ast_matchers::BoundNodes& bound_nodes) {
          return GetNode<Nodes...>(bound_nodes, ids...);
        }));
    return nodes;
  }

  template <class... Nodes, class Matcher, class Node, class... Ids>
  auto NodesFromMatchNode(Matcher matcher, const Node& node, Ids... ids) const {
    auto matches = clang::ast_matchers::match(matcher, node, GetAstContext());
    auto nodes = MakeVector(llvm::map_range(
        matches, [ids...](const clang::ast_matchers::BoundNodes& bound_nodes) {
          return GetNode<Nodes...>(bound_nodes, ids...);
        }));
    return nodes;
  }

  template <class Matcher>
  DeclSet DeclSetFromMatchAST(Matcher matcher, llvm::StringRef id) const {
    auto nodes_from_match = NodesFromMatchAST<clang::Decl>(matcher, id);
    DeclSet node_set{nodes_from_match.begin(), nodes_from_match.end()};
    return node_set;
  }

  template <class Matcher, class Node>
  DeclSet DeclSetFromMatchNode(Matcher matcher, const Node& node,
                               llvm::StringRef id) const {
    auto nodes_from_match = NodesFromMatchNode<clang::Decl>(matcher, node, id);
    return {nodes_from_match.begin(), nodes_from_match.end()};
  }

  template <class Matcher, class Node>
  bool Match(Matcher matcher, const Node& node) const {
    return !clang::ast_matchers::match(matcher, node, GetAstContext()).empty();
  }

  auto GetSourceText(clang::SourceRange source_range) const {
    return clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(source_range), SourceManager(),
        LangOpts());
  }

  VarMatcher Assigned(const ExpressionMatcher& expr_matcher) const {
    // NOLINTNEXTLINE(google-build-using-namespace)
    using namespace clang::ast_matchers;

    // N.B. IsInSet here is not merely an optimization: we CANNOT use
    // hasDescendant in a custom matcher: the memoization is designed to work in
    // a monolithic matcher where the inner matcher outlives the "Finder"
    // object. In a custom matcher that attempts to use hasDescendant, the inner
    // matcher will be temporary that is destroyed with each invocation.
    //
    // As a consequence of this workaround, we cannot preserve any bindings from
    // expr_matcher, so caller aware.
    return IsInSet(DeclSetFromMatchAST(
        AssignTo(declRefExpr(to(varDecl().bind("owner_var"))),
                 IgnoringParenCastFuncs(expr_matcher)),
        "owner_var"));
  }

  VarMatcher InitializedOrAssigned(
      const ExpressionMatcher& expr_matcher) const {
    // NOLINTNEXTLINE(google-build-using-namespace)
    using namespace clang::ast_matchers;

    return anyOf(hasInitializer(expr_matcher), Assigned(expr_matcher));
  }

  VarMatcher RefCountVarInitializedOrAssigned(
      ExpressionMatcher const& expr_matcher) const {
    // NOLINTNEXTLINE(google-build-using-namespace)
    using namespace clang::ast_matchers;

    return allOf(hasType(RefCountPointerType()),
                 InitializedOrAssigned(expr_matcher));
  }
};

clang::TypeLoc IgnoringElaboratedQualified(clang::TypeLoc type_loc);

void CantFail(llvm::Error error) noexcept;

void AnnotateSourceRange(
    clang::SourceRange source_range, clang::SourceRange sub_range,
    llvm::StringRef annotation, const clang::ASTContext& ast_context,
    std::map<std::string, clang::tooling::Replacements>& replacements);

void AnnotateSourceRange(
    clang::SourceRange source_range, llvm::StringRef annotation,
    const clang::ASTContext& ast_context,
    std::map<std::string, clang::tooling::Replacements>& replacements);

clang::TypeLoc GetPointeeLocOfFirstTemplateArg(clang::TypeLoc type_loc);

StatementMatcher HasSourceRange(clang::SourceRange source_range);

__attribute__((const)) DeclarationMatcher RefArrayDecl();

__attribute__((const)) DeclarationMatcher MethodOfHashMap();
template <class... ArgMatchers>
__attribute__((const)) DeclarationMatcher HashMapDecl(ArgMatchers... args) {
  return clang::ast_matchers::classTemplateSpecializationDecl(
      clang::ast_matchers::hasName("::gpos::CHashMap"), args...);
}
template <class... ArgMatchers>
__attribute__((const)) DeclarationMatcher HashMapIterDecl(ArgMatchers... args) {
  return clang::ast_matchers::classTemplateSpecializationDecl(
      clang::ast_matchers::hasName("::gpos::CHashMapIter"), args...);
}
__attribute__((const)) DeclarationMatcher HashMapRefKRefTDecl();
__attribute__((const)) DeclarationMatcher HashMapIterRefKRefTDecl();
__attribute__((const)) TemplateArgumentMatcher RefersToCleanupRelease();

StatementMatcher FieldReferenceFor(DeclarationMatcher const& field_matcher);
StatementMatcher ReleaseCallExpr(ExpressionMatcher const& reference_to_field);
StatementMatcher AddRefOn(ExpressionMatcher const& expr_matcher);

bool IsInMacro(clang::SourceRange source_range);

template <class Parent, class Node>
const Parent* GetParentAs(const Node& node, clang::ASTContext& ast_context) {
  for (auto potential_parent : ast_context.getParents(node))
    if (const auto* parent = potential_parent.template get<Parent>(); parent)
      return parent;
  return nullptr;
}

StatementMatcher StmtIsImmediatelyBefore(const StatementMatcher& rhs);
StatementMatcher StmtIsImmediatelyAfter(const StatementMatcher& lhs);

__attribute__((const)) DeclarationMatcher AutoRefDecl();

}  // namespace orca_tidy
#endif  // ORCATIDY__ASTHELPERS_H_
