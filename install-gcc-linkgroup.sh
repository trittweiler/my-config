#!/bin/bash

if [ $# != 2 ]; then
    echo "USAGE: $0 <version> <priority>" >&2
    exit 1
fi

version="$1"
priority="$2"

for x in /usr/bin/gcc-${version} /usr/bin/g++-${version} /usr/bin/cpp-${version}; do
    if ! test -e ${x}; then
    echo "ERROR: no such file or directory: ${x}" >&2
    exit 1
    fi
done

declare -a gcc_binaries=(
    g++
    # Note: "gcc" is implicitly added as the master.
    gcc-cpp
    gcc-ar
    gcc-nm
    gcc-ranlib
    gcov
    gcov-dump
    gcov-tool
    lto-dump
)

function gen_slaves() {
    local version="$1" dir="$2"
    shift 2
    for name in "$@"; do
        local link=${dir}/${name}
        local path=${link}-${version}
        echo "--slave ${link} ${name} ${path}"
    done
}

# Ubuntu comes with a linkgroup for "cpp" already pointing to
# /lib/cpp. That is why we create an alias `gcc-cpp-<version>` here
# and make that part of our "gcc" linkgroup.
sudo ln -sf /usr/bin/cpp-${version} /usr/bin/gcc-cpp-${version}

sudo update-alternatives \
     --install /usr/bin/gcc gcc /usr/bin/gcc-${version} ${priority} \
     $(gen_slaves ${version} /usr/bin ${gcc_binaries[@]})

echo "done."

set -x
sudo update-alternatives --list gcc
