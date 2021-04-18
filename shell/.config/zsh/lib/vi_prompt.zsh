# Global aliases can break things.
# Unset before using any non-builtins.
[[ -o aliases ]] && _vim_mode_shopt_aliases=1
builtin set -o no_aliases

export VIM_MODE_KEYMAP

autoload -Uz add-zsh-hook
autoload -Uz add-zle-hook-widget

typeset -g -a vim_mode_keymap_funcs=()

vim-mode-precmd           () { vim-mode-handle-event precmd           "$KEYMAP" }
vim-mode-isearch-update   () { vim-mode-handle-event isearch-update   "$KEYMAP" }
vim-mode-isearch-exit     () { vim-mode-handle-event isearch-exit     "$KEYMAP" }
vim-mode-line-pre-redraw  () { vim-mode-handle-event line-pre-redraw  "$KEYMAP" }
vim-mode-line-init        () { vim-mode-handle-event line-init        "$KEYMAP" }

add-zsh-hook precmd vim-mode-precmd

() {
  local w;
  for w in "$@"; do
    add-zle-hook-widget $w vim-mode-$w;
  done
} isearch-exit isearch-update line-pre-redraw line-init

typeset -g vim_mode_keymap_state

vim-mode-handle-event () {
  local hook="$1"
  local keymap="$2"

  case $hook in
    line-init )
      [[ $VIM_MODE_KEYMAP = vicmd ]] && zle && zle vi-cmd-mode
      ;;
    line-pre-redraw )
      # This hook is called (maybe several times) on every action except
      # for the initial prompt drawing
      case $vim_mode_keymap_state in
        '' )
          vim_mode_set_keymap "$keymap"
          ;;
        *-escape )
          vim_mode_set_keymap "${vim_mode_keymap_state%-escape}"
          vim_mode_keymap_state=
          ;;
        *-update )
          # Normal update in isearch mode
          vim_mode_keymap_state=${vim_mode_keymap_state%-update}
          vim_mode_set_keymap isearch
          ;;
        * )
          # ^C was hit during isearch mode!
          vim_mode_set_keymap "$vim_mode_keymap_state"
          vim_mode_keymap_state=
          ;;
      esac
      ;;
    isearch-update )
      if [[ $keymap = vicmd ]]; then
        # This is an abnormal exit from search (like <Esc>)
        vim_mode_keymap_state+='-escape'
      elif [[ $VIM_MODE_KEYMAP != isearch ]]; then
        # Normal update, starting search mode
        vim_mode_keymap_state=${VIM_MODE_KEYMAP}-update
      else
        # Normal update, staying in search mode
        vim_mode_keymap_state+=-update
      fi
      ;;
    isearch-exit )
      if [[ $VIM_MODE_KEYMAP = isearch ]]; then
        # This could be a normal (movement key) exit, but it could also
        # be ^G which behaves almost like <Esc>. So don't trust $keymap.
        vim_mode_keymap_state+='-escape'
      fi

      # Otherwise, we already exited search via abnormal isearch-update,
      # so there is nothing to do here.
      ;;
    precmd )
      # When the prompt is first shown line-pre-redraw does not get called
      # so the state must be initialized here
      vim_mode_keymap_state=
      vim_mode_set_keymap $(vim-mode-initial-keymap)
      ;;
    * )
      # Should not happen
      zle && zle -M "zsh-vim-mode internal error: bad hook $hook"
      ;;
  esac
}

vim_mode_set_keymap () {
  local keymap="$1"

  [[ $keymap = main || $keymap = '' ]] && keymap=viins

  if [[ $keymap = vicmd ]]; then
    local active=${REGION_ACTIVE:-0}
    if [[ $active = 1 ]]; then
      keymap=visual
    elif [[ $active = 2 ]]; then
      keymap=vline
    fi
  elif [[ $keymap = viins ]]; then
    [[ $ZLE_STATE = *overwrite* ]] && keymap=replace
  fi


  [[ $VIM_MODE_KEYMAP = $keymap ]] && return

  VIM_MODE_KEYMAP=$keymap

  local func
  for func in ${vim_mode_keymap_funcs[@]}; do
    ${func} "$keymap"
  done
}

vim-mode-initial-keymap () {
  case ${VIM_MODE_INITIAL_KEYMAP:-viins} in
    last)
      case $VIM_MODE_KEYMAP in
        vicmd|visual|vline) print vicmd ;;
        *)                  print viins ;;
      esac
      ;;
    vicmd)
      print vicmd ;;
    *)
      print viins ;;
  esac
}

# Editing mode indicator - Prompt string

# Unique prefix to tag the mode indicator text in the prompt.
# If ZLE_RPROMPT_INDENT is < 1, zle gets confused if $RPS1 isn't empty but
# printing it doesn't move the cursor.
(( ${ZLE_RPROMPT_INDENT:-1} > 0 )) \
  && vim_mode_indicator_pfx="%837(l,,)" \
  || vim_mode_indicator_pfx="%837(l,, )"

# If mode indicator wasn't setup by theme, define default
vim-mode-set-up-indicators () {
  local indicator=${MODE_INDICATOR_VICMD-${MODE_INDICATOR-DEFAULT}}
  local set=$((
  $+MODE_INDICATOR_VIINS +
  $+MODE_INDICATOR_REPLACE +
  $+MODE_INDICATOR_VICMD +
  $+MODE_INDICATOR_SEARCH +
  $+MODE_INDICATOR_VISUAL +
  $+MODE_INDICATOR_VLINE))

  if [[ -n $indicator || $set > 0 ]]; then
    if (( ! $set )); then
      if [[ $indicator = DEFAULT ]]; then
        MODE_INDICATOR_VIINS='%F{4}I%f'
        MODE_INDICATOR_VICMD='%F{2}N%f'
        MODE_INDICATOR_REPLACE='%F{1}R%f'
        MODE_INDICATOR_SEARCH='%F{5}S%f'
        MODE_INDICATOR_VISUAL='%F{4}V%f'
        MODE_INDICATOR_VLINE='%F{4}V-L%f'
      else
        MODE_INDICATOR_VICMD=$indicator
      fi

      # Replace / Search indicator defaults to viins
      (( $+MODE_INDICATOR_VIINS )) && \
        : ${MODE_INDICATOR_REPLACE=$MODE_INDICATOR_VIINS}
                (( $+MODE_INDICATOR_VIINS )) && \
                  : ${MODE_INDICATOR_SEARCH=$MODE_INDICATOR_VIINS}
      # Visual indicator defaults to vicmd
      (( $+MODE_INDICATOR_VICMD )) && \
        : ${MODE_INDICATOR_VISUAL=$MODE_INDICATOR_VICMD}
                (( $+MODE_INDICATOR_VISUAL )) && \
                  : ${MODE_INDICATOR_VLINE=$MODE_INDICATOR_VISUAL}

      MODE_INDICATOR_PROMPT=${vim_mode_indicator_pfx}${MODE_INDICATOR_VIINS}

      if (( !$+RPS1 )); then
        [[ -o promptsubst ]] && RPS1='${MODE_INDICATOR_PROMPT}' || RPS1="$MODE_INDICATOR_PROMPT"
      fi
    fi
  else
    unset MODE_INDICATOR_PROMPT
  fi
}

vim-mode-update-prompt () {
  local keymap="$1"
  # See if user requested indicators since last time
  (( $+MODE_INDICATOR_PROMPT )) || vim-mode-set-up-indicators
  (( $+MODE_INDICATOR_PROMPT )) || return

  local -A modes=(
  I  ${vim_mode_indicator_pfx}${MODE_INDICATOR_VIINS}
  C  ${vim_mode_indicator_pfx}${MODE_INDICATOR_VICMD}
  R  ${vim_mode_indicator_pfx}${MODE_INDICATOR_REPLACE}
  S  ${vim_mode_indicator_pfx}${MODE_INDICATOR_SEARCH}
  V  ${vim_mode_indicator_pfx}${MODE_INDICATOR_VISUAL}
  L  ${vim_mode_indicator_pfx}${MODE_INDICATOR_VLINE}
  # In case user has changed the mode string since last call, look
  # for the previous value as well as set of current values
  p  ${MODE_INDICATOR_PROMPT}
  )

  # Pattern that will match any value from $modes. Reverse sort, so that
  # if one pattern is a prefix of a longer one, it will be tried after.
  local any_mode=${(j:|:)${(Obu)modes}}

  (( $+RPROMPT )) && : ${RPS1=$RPROMPT}
  local prompts="$PS1 $RPS1"

  case $keymap in
    vicmd)        MODE_INDICATOR_PROMPT=$modes[C] ;;
    replace)      MODE_INDICATOR_PROMPT=$modes[R] ;;
    isearch)      MODE_INDICATOR_PROMPT=$modes[S] ;;
    visual)       MODE_INDICATOR_PROMPT=$modes[V] ;;
    vline)        MODE_INDICATOR_PROMPT=$modes[L] ;;
    main|viins|*) MODE_INDICATOR_PROMPT=$modes[I] ;;
  esac

  if [[ ${(SN)prompts#${~any_mode}} > 0 ]]; then
    PS1=${PS1//${~any_mode}/$MODE_INDICATOR_PROMPT}
    RPS1=${RPS1//${~any_mode}/$MODE_INDICATOR_PROMPT}
  fi

  zle || return
  zle reset-prompt
}

vim-mode-set-up-indicators
vim_mode_keymap_funcs+=vim-mode-update-prompt

# Restore shell option 'aliases'. This must be the last thing here. {{{1
if [[ $_vim_mode_shopt_aliases = 1 ]]; then
  unset _vim_mode_shopt_aliases
  set -o aliases
fi
# }}}

