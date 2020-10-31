#!/bin/bash

if [ $# != 2 ]; then
    echo "USAGE: $0 <version> <priority>" >&2
    exit 1
fi

version="$1"
priority="$2"

for x in /usr/bin/python${version}; do
    if ! test -e ${x}; then
    echo "ERROR: no such file or directory: ${x}" >&2
    exit 1
    fi
done

declare -a python_binaries=(
    # Note: "python3" is implicitly added as the master.
    python3-config
    python3m
    python3m-config
)

function gen_slaves() {
    local version="$1" dir="$2"
    shift 2
    for name in "$@"; do
        local link=${dir}/${name}
        local path=${link/python3/python${version}}
        echo "--slave ${link} ${name} ${path}"
    done
}

sudo update-alternatives \
     --install /usr/bin/python3 python3 /usr/bin/python${version} ${priority} \
     $(gen_slaves ${version} /usr/bin ${python_binaries[@]})

echo "done."

set -x
sudo update-alternatives --list python3
