FROM ubuntu:rolling
RUN apt-get update -q
RUN apt-get install -y cmake ninja-build libclang-dev
COPY entrypoint.bash /entrypoint
ENTRYPOINT /entrypoint
