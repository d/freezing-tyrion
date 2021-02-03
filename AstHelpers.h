#ifndef ORCATIDY__ASTHELPERS_H_
#define ORCATIDY__ASTHELPERS_H_
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace orca_tidy {

static const constexpr llvm::StringRef kOwnerAnnotation = "gpos::owner";
static const constexpr llvm::StringRef kPointerAnnotation = "gpos::pointer";

using clang::ast_matchers::TypeMatcher;

using NamedMatcher = decltype(clang::ast_matchers::hasName(""));
static TypeMatcher AnnotationType(NamedMatcher named_matcher);

__attribute__((const)) TypeMatcher OwnerType();

__attribute__((const)) TypeMatcher PointerType();

__attribute__((const)) TypeMatcher LeakedType();

__attribute__((const)) TypeMatcher AnnotatedType();

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
