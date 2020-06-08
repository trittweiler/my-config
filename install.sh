#!/bin/bash
set -e

source /etc/lsb-release
if ! [ "${DISTRIB_ID}" == "Ubuntu" ]; then
    echo "Error: script only works with Ubuntu" >&2
    exit 1
fi

if ! [ "$UID" -eq 0 ]; then
    exec sudo $0 "$@"
fi

declare -a packages=(
    autoconf
    build-essential
    bear			# LD_PRELOAD tool to generate compile_commands.json
    fonts-croscore              # Cousine font
    clang-9
    clang-format-9
    clang-tidy-9
    clang-tools-9
    clangd-9
    errno                       # convert errno to numbers
    lldb-9
    lld-9
    htop
    libgif-dev			# for compiling Emacs
    libgtk-3-dev		# ditto
    libjpeg-dev			# ditto
    libssl-dev			# ditto
    libxpm-dev			# ditto
    screen
    texinfo			# makeinfo
)

set -x
apt update
apt install -y ${packages[@]}
