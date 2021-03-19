#ifndef ORCATIDY__CONVERTERTEST_H_
#define ORCATIDY__CONVERTERTEST_H_
#include "OrcaTidyTest.h"

#include <string_view>

namespace orca_tidy {
struct ConverterTest : OrcaTidyTest<ConverterTest> {
  static inline const std::string kPreamble = R"C++(
#include <cstdint>    // for uint32_t
#include <typeindex>  // for hash
#include <utility>    // for move

    namespace gpos {
    using ULONG = uint32_t;
    using ULONG_PTR = uintptr_t;
    using BOOL = bool;

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
    using cast_func = T;

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

    template <class K, ULONG (*HashFn)(const K*)>
    struct RefHash;

    template <class K, BOOL (*EqFn)(const K*, const K*)>
    struct RefEq;

    template <class T>
    Ref<T> RefFromNew(T*);

    template <class X, class Y>
    X* dyn_cast(Y*);
    template <class X, class Y>
    Ref<X> dyn_cast(Ref<Y>&&);
    template <class X, class Y>
    Ref<X> dyn_cast(const Ref<Y>&);

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

    template <class T>
    void CleanupRelease(T* elem);

    template <class T, void (*CleanupFn)(T*)>
    class CDynamicPtrArray : public CRefCount<CDynamicPtrArray<T, CleanupFn>> {
     public:
      T* operator[](ULONG) const;
      void Replace(ULONG pos, T* new_elem);
      void Append(T* elem);
    };

    template <class K, class T, ULONG (*HashFn)(const K*),
              BOOL (*EqFn)(const K*, const K*), void (*DestroyKFn)(K*),
              void (*DestroyTFn)(T*)>
    class CHashMap : public CRefCount<
                         CHashMap<K, T, HashFn, EqFn, DestroyKFn, DestroyTFn>> {
     public:
      BOOL Insert(K* key, T* value);
      BOOL Replace(const K* key, T* ptNew);
    };

    template <class K, class T, ULONG (*HashFn)(const K*),
              BOOL (*EqFn)(const K*, const K*), void (*DestroyKFn)(K*),
              void (*DestroyTFn)(T*)>
    class CHashMapIter {
     public:
      CHashMapIter(CHashMap<K, T, HashFn, EqFn, DestroyKFn, DestroyTFn>*);
    };

    template <class T>
    class CAutoRef {
     public:
      explicit CAutoRef(T* = nullptr);
      const CAutoRef& operator=(T*);
    };

    template <class T>
    ULONG HashValue(const T*);

    template <class T>
    ULONG HashPtr(const T*);

    template <class T>
    BOOL EqualPtr(const T* pt1, const T* pt2);

    ULONG HashULongPtr(const ULONG_PTR& key);

    template <class T>
    BOOL Equals(const T* pt1, const T* pt2);

    BOOL EqualULongPtr(const ULONG_PTR& key_left, const ULONG_PTR& key_right);

    }  // namespace gpos

    template <class T>
    struct std::hash<gpos::Ref<T>>;

    struct T : gpos::CRefCount<T> {
      static gpos::ULONG UlHash(const T*);
      static gpos::BOOL FEquals(const T*, const T*);
    };
  )C++";

  static std::string runToolOverCode(std::string code);
};

}  // namespace orca_tidy
#endif  // ORCATIDY__CONVERTERTEST_H_
