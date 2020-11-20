_timer_preexec() {
  timer=${timer:-$SECONDS}
  TIMER_MSG=${TIMER_MSG-"Time: %s"}
  export TIMER_COMMAND=""
}

_timer_precmd() {
  if [ $timer ]; then
    timer_show=$(($SECONDS - $timer))
    if [ -n "$TTY" ] && [ $timer_show -ge ${TIMER_MIN_SEC:-1} ]; then
      export TIMER_COMMAND="$timer_show"
      if [ ! -z ${TIMER_MSG} ]; then
        timer_command
      fi
    fi
    unset timer
  fi
}

timer_command() {
  if [ -n "$TIMER_COMMAND" ]; then
    hours=$(($TIMER_COMMAND/3600))
    min=$(($TIMER_COMMAND/60))
    sec=$(($TIMER_COMMAND%60))
    if [ "$TIMER_COMMAND" -le 60 ]; then
            timer_show="$fg[green]$TIMER_COMMAND s."
        elif [ "$TIMER_COMMAND" -gt 60 ] && [ "$TIMER_COMMAND" -le 180 ]; then
            timer_show="$fg[yellow]$min min. $sec s."
        else
            if [ "$hours" -gt 0 ]; then
                min=$(($min%60))
                timer_show="$fg[red]$hours h. $min min. $sec s."
            else
                timer_show="$fg[red]$min min. $sec s."
            fi
        fi
        printf "${TIMER_MSG}\n" "$timer_show"
  fi
}

precmd_functions+=(_timer_precmd)
preexec_functions+=(_timer_preexec)
