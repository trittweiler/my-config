#!/bin/bash
if ! [ "$UID" -eq 0 ]; then
    exec sudo $0 "$@"
fi

declare -a packages=(
    fonts-croscore              # Cousine font
    screen
)

set -x
apt update
apt install -y ${packages[@]}
