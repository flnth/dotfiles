#!/usr/bin/env sh
# Usage: install-system-packages.sh package_file
packages=$(cat $1 | egrep -v "(^#.*|^$)" | tr '\n' ' ')
echo "\n\033[0;35mInstalling packages:\033[36m $packages\033[0m"
sudo apt install -y $packages
