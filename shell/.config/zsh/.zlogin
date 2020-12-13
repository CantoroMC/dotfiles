if [[ -z $DISPLAY ]] && (( $EUID != 0 )) && [[ "$(tty)" = "/dev/tty1" ]]; then
  startx "$XDG_CONFIG_HOME/X11/xinitrc" 1>~/.xsession-errors 2>&1 &
fi
