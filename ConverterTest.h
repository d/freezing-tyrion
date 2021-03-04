#ifndef ORCATIDY__CONVERTERTEST_H_
#define ORCATIDY__CONVERTERTEST_H_
#include "OrcaTidyTest.h"

#include <string_view>

namespace orca_tidy {
struct ConverterTest : OrcaTidyTest<ConverterTest> {
  static inline const std::string kPreamble = R"C++(
#include <utility>  // for move

    namespace gpos {
    template <class Derived>
    struct CRefCount {
      void Release();
      void AddRef();
    };

    template <class T>
    void SafeRelease(CRefCount<T> *);

    template <class T>
    using owner = T;

    template <class T>
    using pointer = T;
    }  // namespace gpos

    struct T : gpos::CRefCount<T> {};
  )C++";

  static std::string runToolOverCode(std::string code);
};

}  // namespace orca_tidy
#endif  // ORCATIDY__CONVERTERTEST_H_
