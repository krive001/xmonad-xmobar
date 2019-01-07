#!/bin/bash

# Powerlevel9k

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

# Copy

cp .zshrc ~
cp .Xresources ~
sudo cp HackRegularNerdFontComplete.ttf /usr/share/fonts/TTF/
sudo cp Hack\ Regular\ Nerd\ Font\ Complete\ Mono.ttf /usr/share/fonts/TTF/
cp icons.zsh ~/.oh-my-zsh/custom/themes/powerlevel9k/functions/

# Refresh font cache

sudo fc-cache -fv

# Set terminal font Hack Nerd Font Regular

mkdir ~/.config/mc

cp mc/* ~/.config/mc

