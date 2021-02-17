#ifndef ORCATIDY__ASTHELPERS_H_
#define ORCATIDY__ASTHELPERS_H_
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace orca_tidy {

static const constexpr llvm::StringRef kOwnerAnnotation = "gpos::owner";
static const constexpr llvm::StringRef kPointerAnnotation = "gpos::pointer";
static const constexpr llvm::StringRef kCastAnnotation = "gpos::cast";

using ExpressionMatcher = decltype(clang::ast_matchers::nullPointerConstant());
using VarMatcher = decltype(clang::ast_matchers::hasLocalStorage());
using ParamMatcher = decltype(clang::ast_matchers::hasDefaultArgument());
using clang::ast_matchers::DeclarationMatcher;
using clang::ast_matchers::StatementMatcher;
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

StatementMatcher Deref(const ExpressionMatcher& expr);
StatementMatcher AddrOf(const ExpressionMatcher& expr);

__attribute__((const)) DeclarationMatcher SingleDecl();

StatementMatcher AssignTo(const ExpressionMatcher& lhs);

StatementMatcher AssignTo(const ExpressionMatcher& lhs,
                          const ExpressionMatcher& rhs);

AST_MATCHER(clang::CXXMethodDecl, IsStatic) { return Node.isStatic(); }

using DeclSet = llvm::DenseSet<const clang::Decl*>;
AST_MATCHER_P(clang::Decl, IsInSet, DeclSet, nodes) {
  return nodes.contains(&Node);
}

clang::QualType StripElaborated(clang::QualType qual_type);
TypeMatcher IgnoringElaborated(TypeMatcher type_matcher);

bool IsCastFunc(const clang::Decl* decl);
const clang::Expr* IgnoreCastFuncs(const clang::Expr* expr);
AST_MATCHER_P(clang::Expr, IgnoringCastFuncs, ExpressionMatcher,
              inner_matcher) {
  return inner_matcher.matches(*IgnoreCastFuncs(&Node), Finder, Builder);
}

const clang::Expr* IgnoreParenCastFuncs(const clang::Expr* expr);
AST_MATCHER_P(clang::Expr, IgnoringParenCastFuncs, ExpressionMatcher,
              inner_matcher) {
  return inner_matcher.matches(*IgnoreParenCastFuncs(&Node), Finder, Builder);
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

template <class Derived>
struct NodesFromMatchBase {
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
  /// for (const auto* v = NodesFromMatch<VarDecl>(varDecl().bind("v"))) {
  /// ...
  /// }
  /// \endcode
  ///
  /// In addition, when you pass in two or more bindings, the returned sequence
  /// can be iterated through structured binding:
  ///
  /// \code
  /// for (auto [var, arg]: NodesFromMatch<VarDecl, Expr>(callExpr(
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
  auto NodesFromMatch(Matcher matcher, Ids... ids) const {
    auto matches = clang::ast_matchers::match(
        matcher, static_cast<const Derived*>(this)->AstContext());
    auto nodes = MakeVector(llvm::map_range(
        matches, [ids...](const clang::ast_matchers::BoundNodes& bound_nodes) {
          return GetNode<Nodes...>(bound_nodes, ids...);
        }));
    return nodes;
  }

  template <class Matcher>
  DeclSet DeclSetFromMatch(Matcher matcher, llvm::StringRef id) const {
    auto nodes_from_match = NodesFromMatch<clang::Decl>(matcher, id);
    DeclSet node_set{nodes_from_match.begin(), nodes_from_match.end()};
    return node_set;
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
    return IsInSet(
        DeclSetFromMatch(AssignTo(declRefExpr(to(varDecl().bind("owner_var"))),
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

}  // namespace orca_tidy
#endif  // ORCATIDY__ASTHELPERS_H_
