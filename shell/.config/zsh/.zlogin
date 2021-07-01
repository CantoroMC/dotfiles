if [[ -z $DISPLAY ]] && (( $EUID != 0 )) && [[ "$(tty)" = "/dev/tty2" ]]; then
  startx "$XDG_CONFIG_HOME/X11/xinitrc" 1> "$XDG_DATA_HOME/xorg/xsession-errors" 2>&1 &
fi
