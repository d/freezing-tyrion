FROM ubuntu:rolling
RUN apt-get update -q
RUN apt-get install -y ninja-build libclang-dev
COPY entrypoint.bash /entrypoint
ENTRYPOINT /entrypoint
