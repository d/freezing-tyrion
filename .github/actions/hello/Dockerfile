FROM ubuntu:rolling
RUN apt-get update -q
RUN apt-get install -y g++ cmake ninja-build llvm-dev clang libclang-dev
COPY entrypoint.bash /entrypoint
ENTRYPOINT /entrypoint
