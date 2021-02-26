#ifndef ORCATIDY__ANNOTATETEST_H_
#define ORCATIDY__ANNOTATETEST_H_
#include "OrcaTidyTest.h"
namespace orca_tidy {
class AnnotateTest : public OrcaTidyTest<AnnotateTest> {
 public:
  explicit AnnotateTest(ActionOptions action_options)
      : action_options_(action_options) {}

  constexpr static const char* const kPreamble = R"C++(
#include <cstdint>
#include "CRefCount.h"
#include "owner.h"
    namespace gpos {
    using ULONG = uint32_t;
    using BOOL = bool;

    template <class T>
    void CleanupRelease(T *elem);

    template <class T, void (*CleanupFn)(T *)>
    class CDynamicPtrArray : public CRefCount<CDynamicPtrArray<T, CleanupFn>> {
     public:
      T *operator[](ULONG) const;
      void Replace(ULONG pos, T *new_elem);
      void Append(T *elem);
    };

    template <class K, class T, ULONG (*HashFn)(const K *),
              BOOL (*EqFn)(const K *, const K *), void (*DestroyKFn)(K *),
              void (*DestroyTFn)(T *)>
    class CHashMap : public CRefCount<
                         CHashMap<K, T, HashFn, EqFn, DestroyKFn, DestroyTFn>> {
     public:
      BOOL Insert(K *key, T *value);
      BOOL Replace(const K *key, T *ptNew);
    };
    }  // namespace gpos

    struct T : gpos::CRefCount<T> {};
    using U = T;
    struct S : U {};

    void Assert(bool);
    template <class T1, class... Ts>
    void Sink(T1, Ts...);
  )C++";

  std::string runToolOverCode(std::string code);

 protected:
  const orca_tidy::ActionOptions action_options_;
};

struct BaseTest : AnnotateTest {
  BaseTest() : AnnotateTest({true, false}) {}
};

struct PropagateTest : AnnotateTest {
  PropagateTest() : AnnotateTest({false, true}) {}
};

}  // namespace orca_tidy
#endif  // ORCATIDY__ANNOTATETEST_H_
