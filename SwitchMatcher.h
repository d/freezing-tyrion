// This file implements Switch() matcher. To achieve that this file commits the
// cardinal sin of depending directly on clang::ast_matchers::internal API's.

#ifndef ORCATIDY__SWITCHMATCHER_H_
#define ORCATIDY__SWITCHMATCHER_H_

#include "clang/ASTMatchers/ASTMatchers.h"

namespace orca_tidy {
namespace internal {
template <class... CaseThens, std::size_t... I>
auto CaseThenTupleTypeHelper(std::index_sequence<I...>) -> std::tuple<
    std::tuple<std::tuple_element_t<I * 2, std::tuple<CaseThens...>>,
               std::tuple_element_t<I * 2 + 1, std::tuple<CaseThens...>>>...>;

template <class... CaseThens>
using CaseThenTupleType = decltype(CaseThenTupleTypeHelper<CaseThens...>(
    std::make_index_sequence<sizeof...(CaseThens) / 2>{}));

template <class DefaultMatcher, class... CaseThens>
class SwitchBuilder {
  static constexpr auto N = sizeof...(CaseThens) / 2;
  template <class T>
  using Matcher = clang::ast_matchers::internal::Matcher<T>;

  template <class T>
  using ArrayOfMatchers = std::array<std::tuple<Matcher<T>, Matcher<T>>, N>;

  using ASTMatchFinder = clang::ast_matchers::internal::ASTMatchFinder;
  using BoundNodesTreeBuilder =
      clang::ast_matchers::internal::BoundNodesTreeBuilder;

  DefaultMatcher default_matcher_;
  CaseThenTupleType<CaseThens...> case_thens_;

  template <class T>
  class SwitchMatcher
      : public clang::ast_matchers::internal::MatcherInterface<T> {
    Matcher<T> default_matcher_;
    std::array<std::tuple<Matcher<T>, Matcher<T>>, N> case_thens_;

   public:
    SwitchMatcher(Matcher<T> default_matcher,
                  std::array<std::tuple<Matcher<T>, Matcher<T>>, N> case_thens)
        : default_matcher_(std::move(default_matcher)),
          case_thens_(std::move(case_thens)) {}

    bool matches(const T& Node, ASTMatchFinder* Finder,
                 BoundNodesTreeBuilder* Builder) const override {
      for (auto [c, t] : case_thens_) {
        auto Result = *Builder;
        if (c.matches(Node, Finder, &Result)) {
          *Builder = std::move(Result);
          return t.matches(Node, Finder, Builder);
        }
      }
      return default_matcher_.matches(Node, Finder, Builder);
    }
  };

  template <class T, size_t... I>
  ArrayOfMatchers<T> CaseThenMatchers(std::index_sequence<I...>) const {
    return ArrayOfMatchers<T>{std::get<I>(case_thens_)...};
  }

 public:
  explicit SwitchBuilder(DefaultMatcher&& default_matcher,
                         CaseThenTupleType<CaseThens...> case_thens)
      : default_matcher_(std::move(default_matcher)),
        case_thens_(std::move(case_thens)) {}

  template <class C, class T>
  SwitchBuilder<DefaultMatcher, CaseThens..., C, T> Case(C&& c, T&& t) && {
    return SwitchBuilder<DefaultMatcher, CaseThens..., C, T>{
        std::move(default_matcher_),
        std::tuple_cat(std::move(case_thens_),
                       std::tuple<std::tuple<C, T>>{
                           {std::forward<C>(c), std::forward<T>(t)}})};
  }

  template <class D>
  SwitchBuilder<D, CaseThens...> Default(D&& d) && {
    return SwitchBuilder<D, CaseThens...>{std::forward<D>(d),
                                          std::move(case_thens_)};
  }

  template <class T>
  operator Matcher<T>() const {
    return Matcher<T>{new SwitchMatcher<T>{
        default_matcher_, CaseThenMatchers<T>(std::make_index_sequence<N>{})}};
  }
};

template <class T>
struct FalseMatcherImpl : clang::ast_matchers::internal::MatcherInterface<T> {
  bool matches(const T& Node,
               clang::ast_matchers::internal::ASTMatchFinder* Finder,
               clang::ast_matchers::internal::BoundNodesTreeBuilder* Builder)
      const override {
    return false;
  }
};

struct FalseMatcher {
  template <typename T>
  operator clang::ast_matchers::internal::Matcher<T>() const {
    return clang::ast_matchers::internal::Matcher<T>(new FalseMatcherImpl<T>{});
  }
};

}  // namespace internal

/// A switch()-like matcher facility whose cases and "then" actions are AST
/// matchers.
///
/// The matcher first matches a node against all cases in order. The node is
/// matched against the "then" matcher of the first matched case.
///
/// For example:
///
/// \code
/// Switch().Case(case1,then1).Case(case2,then2).Default(default_matcher)
/// \endcode
///
/// If a node matches \c case1, we return the match result of \c then1. If a
/// node matches neither \c case1 or \c case2, we return the match result of \c
/// default_matcher. If omitted, the default_matcher never matches.
///
/// This is different from
///
/// \code
/// anyOf(case1,then1,case2,then2,default_matcher)
/// \endcode
///
/// because if a node matches all of \c case1, \c case2, and \c then2 but
/// doesn't match \c then1, \c Switch() would return false, whereas \c anyOf
/// returns true.
inline auto Switch() {
  return internal::SwitchBuilder<internal::FalseMatcher>{{}, {}};
}
}  // namespace orca_tidy

#endif  // ORCATIDY__SWITCHMATCHER_H_
