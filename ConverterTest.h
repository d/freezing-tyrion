#ifndef ORCATIDY__CONVERTERTEST_H_
#define ORCATIDY__CONVERTERTEST_H_
#include "OrcaTidyTest.h"

#include <string_view>

namespace orca_tidy {
struct ConverterTest : OrcaTidyTest<ConverterTest> {
  static inline const std::string kPreamble = R"C++(
#include <typeindex>  // for hash
#include <utility>    // for move

    namespace gpos {
    template <class Derived>
    struct CRefCount {
      void Release();
      void AddRef();
    };

    template <class T>
    void SafeRelease(CRefCount<T>*);

    template <class T>
    using owner = T;
    template <class T>
    using pointer = T;
    template <class T>
    using leaked = T;
    template <class T>
    using cast = T;

    template <class T>
    class Ref {
      T* p_;

     public:
      Ref(T* p = nullptr);
      ~Ref();
      Ref(const Ref&);
      Ref(Ref&&);
      Ref& operator=(const Ref&);
      Ref& operator=(Ref&&);
      explicit operator bool() const;
      T* operator->() const;
      T& operator*() const;
      T* get() const;

      template <class U>
      Ref<U> getAs() &;
      template <class U>
      Ref<U> getAs() &&;
    };

    template <class T>
    Ref<T> RefFromNew(T*);

    template <class T>
    class CAutoP {
     public:
      CAutoP const& operator=(T*);
    };

    template <class T, class K>
    class CCache {
     public:
    };
    template <class T, class K>
    class CCacheAccessor {};
    }  // namespace gpos

    template <class T>
    struct std::hash<gpos::Ref<T>>;

    struct T : gpos::CRefCount<T> {};
  )C++";

  static std::string runToolOverCode(std::string code);
};

}  // namespace orca_tidy
#endif  // ORCATIDY__CONVERTERTEST_H_
