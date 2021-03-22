cmake_minimum_required(VERSION 3.12)
project(OrcaTidy CXX)

set(CMAKE_CXX_STANDARD 20)
option(ORCA_TIDY_CCACHE "Whether to enable ccache" ON)
if(ORCA_TIDY_CCACHE)
  set(CMAKE_CXX_COMPILER_LAUNCHER ccache)
endif()
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG HINTS ${LLVM_INSTALL_PREFIX}/lib/cmake/clang)
message(STATUS "LLVM version: " ${LLVM_CONSIDERED_VERSIONS})
message(STATUS "Clang config: " ${Clang_CONSIDERED_CONFIGS})

add_executable(ctors-saving-refs ConstructorsSavingReferences.cc)

target_link_libraries(ctors-saving-refs clangASTMatchers clangTooling)
target_include_directories(ctors-saving-refs PRIVATE ${LLVM_INCLUDE_DIR})

add_library(orca_tidy STATIC OrcaTidy.cc IncludeFixer.cc Converter.cc
                             AstHelpers.cc)
target_include_directories(orca_tidy PUBLIC ${LLVM_INCLUDE_DIR})
target_link_libraries(orca_tidy PUBLIC clangTooling)

add_executable(annotate OrcaTidyMain.cc)
target_link_libraries(annotate orca_tidy)

set(INSTALL_GTEST
    OFF
    CACHE BOOL "")
add_subdirectory(vendor/googletest EXCLUDE_FROM_ALL)

add_executable(
  orca-tidy-test
  OrcaTidyTest.cc
  FunctionPointerTest.cc
  BaseTest.cc
  RetVarTest.cc
  AddRefReturnTest.cc
  PointerVarTest.cc
  TailCallTest.cc
  AnnotateTest.cc
  FieldTest.cc
  ConstPointersTest.cc
  CastTest.cc
  BoolFuncTest.cc
  MultiDeclTest.cc
  OutputParamTest.cc
  OwnerVarTest.cc
  ConverterTest.cc
  ConvertAnnotationTest.cc
  ConvertCcacheTest.cc
  OwnerToPointerTest.cc
  CastingTest.cc
  ConvertRefArrayTest.cc
  ConvertHashMapTest.cc
  ConvertAutoRefTest.cc
  ConvertHashSetTest.cc)
target_link_libraries(orca-tidy-test orca_tidy gmock_main)

include(GoogleTest)
enable_testing()
gtest_discover_tests(orca-tidy-test)
