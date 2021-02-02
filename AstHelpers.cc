#include "AstHelpers.h"

// NOLINTNEXTLINE(google-build-using-namespace)
using namespace clang::ast_matchers;

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
}  // namespace orca_tidy
