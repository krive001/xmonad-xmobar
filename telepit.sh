#!/bin/bash

# Compton
sudo pacman -S xmonad xmonad-contrib xmonad-utils xmobar xorg-xmessage xdotool compton pavucontrol trayer pacman-contrib notify-osd ttf-font-awesome numlockx --noconfirm --needed

# Font Awesome
yay -S ttf-font-awesome-4 checkupdates-aur --noconfirm --needed

# Copy
cp -r .xmonad/* $HOME/.xmonad/
sudo cp usr/local/bin/* /usr/local/bin/

# Copy other
#cp .stalonetrayrc $HOME/
cp .Xresources $HOME/
cp .xmobarrc $HOME/

# Oh-my-zsh
cd $HOME/xmonad-xmobar/powerlevel9k/
sh install.sh


