#!/bin/bash

if [ $# != 2 ]; then
    echo "USAGE: $0 <version> <priority>" >&2
    exit 1
fi

version="$1"
priority="$2"

for x in /usr/lib/llvm-${version} /usr/bin/clang-${version}; do
    if ! test -e ${x}; then
    echo "ERROR: no such file or directory: ${x}" >&2
    exit 1
    fi
done

declare -a llvm_binaries=(
    llvm-addr2line
    llvm-ar
    llvm-as
    llvm-bcanalyzer
    llvm-cat
    llvm-cfi-verify
    llvm-config
    llvm-cov
    llvm-c-test
    llvm-cvtres
    llvm-cxxdump
    llvm-cxxfilt
    llvm-cxxmap
    llvm-diff
    llvm-dis
    llvm-dlltool
    llvm-dsymtool
    llvm-dwarfdump
    llvm-dwp
    llvm-elfabi
    llvm-exegesis
    llvm-extract
    llvm-jitlink
    llvm-lib
    llvm-link
    llvm-lipo
    llvm-lto2
    llvm-lto
    llvm-mc
    llvm-mca
    llvm-mcmarkup
    llvm-modextract
    llvm-mt
    llvm-nm
    llvm-objcopy
    llvm-objdump
    llvm-opt-report
    llvm-pdbutil
    llvm-PerfectShuffle
    llvm-profdata
    llvm-ranlib
    llvm-rc
    llvm-readelf
    llvm-readobj
    llvm-rtdyld
    llvm-size
    llvm-split
    llvm-stress
    llvm-strings
    llvm-strip
    llvm-symbolizer
    llvm-tblgen
    llvm-undname
    llvm-xray
)

declare -a clang_binaries=(
    clang++
    clang-apply-replacements
    clang-change-namespace
    clang-check
    clang-cl
    clang-cpp
    clangd
    clang-extdef-mapping
    clang-format
    clang-format-diff
    clang-import-test
    clang-include-fixer
    clang-offload-bundler
    clang-query
    clang-refactor
    clang-rename
    clang-reorder-fields
    clang-scan-deps
    clang-tidy
    scan-build
    scan-build-py
    scan-view
    bugpoint
    c-index-test
    diagtool
    find-all-symbols
    git-clang-format
    hmaptool
    modularize
    obj2yaml
    opt
    sancov
    sanstats
    verify-uselistorder
    wasm-ld
    yaml2obj
    yaml-bench
    lld
    lld-link
    lli-child-target
    lli
    lldb
    lldb-argdumper
    lldb-mi
    lldb-server
    lldb-test
    lldb-vscode
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


sudo update-alternatives \
     --install /usr/lib/llvm llvm /usr/lib/llvm-${version} ${priority} \
     $(gen_slaves ${version} /usr/bin ${llvm_binaries[@]})

sudo update-alternatives \
     --install /usr/bin/clang clang /usr/bin/clang-${version} ${priority} \
     $(gen_slaves ${version} /usr/bin ${clang_binaries[@]})

echo "done."

set -x
sudo update-alternatives --list llvm
sudo update-alternatives --list clang
