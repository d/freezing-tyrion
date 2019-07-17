cmake_minimum_required(VERSION 3.5)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG
    HINTS ${LLVM_INSTALL_PREFIX}/lib/cmake/clang
    )
