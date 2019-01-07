#!/bin/bash

set -e

# Install zsh

sudo pacman -S zsh --noconfirm --needed

# oh-my-zsh install

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

