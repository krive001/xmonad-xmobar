#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#Find out your monitor name with xrandr or arandr (save and you get this line)
#xrandr --output HDMI1 --primary --mode 1920x1024 --pos 0x0 --rotate normal
#xrandr --output DP2 --primary --mode 1920x1080 --rate 60.00 --output LVDS1 --off &
#xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
#xrandr --output HDMI1 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off

#$HOME/.config/polybar/launch.sh &

#change your keyboard if you need it
setxkbmap -layout hu

#Some ways to set your wallpaper besides variety or nitrogen
feh --bg-scale ~/.xmonad/wall1.jpg &

#conky -c $HOME/.xmonad/scripts/system-overview &
#run variety &
run nm-applet &
#run pamac-tray &
#run xfce4-power-manager &
numlockx on &
#blueberry-tray &
compton --config $HOME/.xmonad/scripts/compton.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
#nitrogen --restore &
#run caffeine &
#run vivaldi-stable &
#run firefox &
#run thunar &
#run dropbox &
#run insync start &
#run discord &
#run spotify &
#run atom &
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 8 --height 35 --transparent true  --alpha 255 --iconspacing 5  &
#run stalonetray &

run trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --height 35 --transparent true --iconspacing 7  --alpha 0 --tint 0x000000 --widthtype request --monitor 1 &
