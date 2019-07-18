cmake_minimum_required(VERSION 3.5)
project(OrcaTidy CXX)

set(CMAKE_CXX_STANDARD 14)
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG
    HINTS ${LLVM_INSTALL_PREFIX}/lib/cmake/clang
    )

add_executable(ctors-saving-refs main.cc)
target_link_libraries(ctors-saving-refs clangASTMatchers clangTooling)
target_include_directories(ctors-saving-refs
    PRIVATE
    ${LLVM_INCLUDE_DIR}
    )
add_custom_command(TARGET ctors-saving-refs
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ctors-saving-refs bin/ctors-saving-refs
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${CLANG_INSTALL_PREFIX}/include include
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${CLANG_INSTALL_PREFIX}/lib lib
    BYPRODUCTS bin/ctors-saving-refs include
    COMMENT "work around a Clang idiosyncracy"
    )
