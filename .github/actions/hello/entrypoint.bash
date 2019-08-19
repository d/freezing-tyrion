#!/bin/bash

set -e -u -o pipefail

_main() {
	cmake -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -H. -Bbuild.release
	ninja -C build.release
}

_main "$@"
