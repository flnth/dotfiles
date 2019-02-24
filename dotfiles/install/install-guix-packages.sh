#!/usr/bin/env sh
# Usage: install-guix-packages.sh package_file
packages=$(cat $1 | egrep -v "(^#.*|^$)" | tr '\n' ' ')
echo "\n\033[0;35mInstalling guix packages:\033[36m $packages\033[0m"
guix package -i $packages
