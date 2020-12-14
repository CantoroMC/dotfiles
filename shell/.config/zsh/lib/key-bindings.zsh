# Global aliases can break things.
# Unset before using any non-builtins.
[[ -o aliases ]] && _vim_mode_shopt_aliases=1
builtin set -o no_aliases

# create a zkbd compatible hash {{{1
# https://jlk.fjfi.cvut.cz/arch/manpages/man/user_caps.5

typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"
key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"
key[Control-Delete]="${terminfo[kDC5]}"

# }}}

# Key Bindings {{{1

bindkey -v

# Widgets {{{2

# Native {{{3
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N down-line-or-beginning-search
zle -N up-line-or-beginning-search

autoload -Uz edit-command-line
zle -N edit-command-line
# }}}

# Directory Navigation {{{3
_cdUndoDir() {
  popd
  zle reset-prompt
  print
  ls
  zle reset-prompt
}

_cdParentKey() {
  pushd ..
  zle reset-prompt
  print
  ls
  zle reset-prompt
}

zle -N _cdUndoDir
zle -N _cdParentKey
# }}}

# Commands: {{{3
_exit-cmd () {
  zle clear-screen
  bye
}
zle -N _exit-cmd

_ranger_cmd () {
  BUFFER="ranger"
  zle end-of-line
  zle accept-line
}
zle -N _ranger_cmd

_tmux_cmd () {
  BUFFER="tmux"
  zle end-of-line
  zle accept-line
}
zle -N _tmux_cmd
# }}}

# }}}

# Kzbd compatible {{{2

[[ -n "${key[Home]}"           ]] && bindkey -M viins "${key[Home]}"           beginning-of-line
[[ -n "${key[Home]}"           ]] && bindkey -M vicmd "${key[Home]}"           beginning-of-line
[[ -n "${key[End]}"            ]] && bindkey -M viins "${key[End]}"            end-of-line
[[ -n "${key[End]}"            ]] && bindkey -M vicmd "${key[End]}"            end-of-line
[[ -n "${key[Insert]}"         ]] && bindkey -M viins "${key[Insert]}"         overwrite-mode
[[ -n "${key[Backspace]}"      ]] && bindkey -M viins "${key[Backspace]}"      backward-delete-char
[[ -n "${key[Backspace]}"      ]] && bindkey -M vicmd "${key[Backspace]}"      backward-delete-char
[[ -n "${key[Delete]}"         ]] && bindkey -M viins "${key[Delete]}"         delete-char
[[ -n "${key[Delete]}"         ]] && bindkey -M vicmd "${key[Delete]}"         delete-char
[[ -n "${key[Left]}"           ]] && bindkey -M viins "${key[Left]}"           backward-char
[[ -n "${key[Right]}"          ]] && bindkey -M viins "${key[Right]}"          forward-char
[[ -n "${key[Control-Delete]}" ]] && bindkey -M viins "${key[Control-Delete]}" kill-word
[[ -n "${key[Control-Delete]}" ]] && bindkey -M vicmd "${key[Control-Delete]}" kill-word
[[ -n "${key[Control-Left]}"   ]] && bindkey -M viins "${key[Control-Left]}"   backward-word
[[ -n "${key[Control-Left]}"   ]] && bindkey -M vicmd "${key[Control-Left]}"   backward-word
[[ -n "${key[Control-Right]}"  ]] && bindkey -M viins "${key[Control-Right]}"  forward-word
[[ -n "${key[Control-Right]}"  ]] && bindkey -M vicmd "${key[Control-Right]}"  forward-word
[[ -n "${key[Up]}"             ]] && bindkey -M viins "${key[Up]}"             up-line-or-history
[[ -n "${key[Up]}"             ]] && bindkey -M vicmd "${key[Up]}"             up-line-or-history
[[ -n "${key[Down]}"           ]] && bindkey -M viins "${key[Down]}"           down-line-or-history
[[ -n "${key[Down]}"           ]] && bindkey -M vicmd "${key[Down]}"           down-line-or-history
[[ -n "${key[PageUp]}"         ]] && bindkey -M viins "${key[PageUp]}"         up-line-or-beginning-search
[[ -n "${key[PageDown]}"       ]] && bindkey -M viins "${key[PageDown]}"       down-line-or-beginning-search
[[ -n "${key[Shift-Tab]}"      ]] && bindkey -M viins "${key[Shift-Tab]}"      reverse-menu-complete

# }}}

# Ctrl+Key {{{2
bindkey -M viins    '^A'    _tmux_cmd
bindkey -M viins    '^B'    exchange-point-and-mark
bindkey -M viins    '^E'    expand-cmd-path
bindkey -M viins    '^F'    accept-and-infer-next-history
bindkey -M viins    '^K'    vi-quoted-insert
bindkey -M viins    '^G'    menu-complete
bindkey -M viins    '^I'    fzf-tab-complete
bindkey -M viins    '^L'    clear-screen
bindkey -M viins    '^N'    history-substring-search-down
bindkey -M viins    '^P'    history-substring-search-up
bindkey -M viins    '^Q'    _exit-cmd
bindkey -M vicmd    '^Q'    _exit-cmd
# bindkey -M viins    '^R'    fzf-history-widget
# bindkey -M viins    '^T'    fzf-file-widget
bindkey -M viins    '^V'    edit-command-line
bindkey -M vicmd    '^V'    edit-command-line
bindkey -M viins    '^Y'    copy-prev-shell-word
bindkey -M viins    ' '     magic-space
bindkey -M viins    '^B^N' _cdUndoDir
bindkey -M vicmd    '^B^N' _cdUndoDir
bindkey -M viins    '^B^M' _cdParentKey
bindkey -M vicmd    '^B^M' _cdParentKey
bindkey -M viins    '^X^L' _ranger_cmd
bindkey -M vicmd    '^X^L' _ranger_cmd
bindkey -M viins -s '^X^H' 'fzf-man\n'
bindkey -M vicmd -s '^X^H' 'fzf-man\n'
bindkey -M viins -s '^X^F' 'rcd\n'
bindkey -M vicmd -s '^X^F' 'rcd\n'
# }}}

# Alt-Key {{{2
# bindkey -M viins    '\ec'  fzf-cd-widget
bindkey -M viins    '\ea'  beginning-of-line
bindkey -M viins    '\ee'  end-of-line
# }}}

# Enable surround text-object {{{2

# Select bracketed: {{{3
autoload -U select-bracketed
zle -N select-bracketed

for m in visual viopp; do
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $m $c select-bracketed
  done
done
# }}}

# Select quoted: {{{3
autoload -U select-quoted
zle -N select-quoted

for m in visual viopp; do
  for c in {a,i}{\',\",\`}; do
    bindkey -M $m $c select-quoted
  done
done
# }}}

# Surround: {{{3
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround

bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround
bindkey -M visual S add-surround
# }}}

# }}}

# Additional fzf + git integration {{{2

# Auxiliary functions {{{3
is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --height 50% "$@" --border
}

join-lines() {
  local item
  while read item; do
    echo -n "${(q)item}"
  done
}

bind-git-helper() {
  local c
  for c in $@; do
    eval "fzf-g$c-widget() { local result=\$(_g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-g$c-widget"
    eval "bindkey '^g^$c' fzf-g$c-widget"
  done
}

# }}}

# Widgets: {{{3
_gf() {
  is_in_git_repo || return
  git -c color.status=always status --short |
    fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
    cut -c4- | sed 's/.* -> //'
  }

_gb() {
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
    fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
    sed 's/^..//' | cut -d' ' -f1 |
    sed 's#^remotes/##'
  }

_gt() {
  is_in_git_repo || return
  git tag --sort -version:refname |
    fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {} | head -'$LINES
  }

_go() {
  is_in_git_repo || return
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
    fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
    grep -o "[a-f0-9]\{7,\}"
  }

_gr() {
  is_in_git_repo || return
  git remote -v | awk '{print $1 "\t" $2}' | uniq |
    fzf-down --tac \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' |
    cut -d$'\t' -f1
  }
# }}}

bind-git-helper f b t o r

unset -f bind-git-helper

# }}}

# More fzf integration {{{2
autoload fzf-bindkey fzf-kill

zle -N fzf-bindkey
zle -N fzf-kill

bindkey -M viins '^X^B' fzf-bindkey
bindkey -M viins '^X^K' fzf-kill
# }}}

# }}}

# Prompt: {{{1

# Identifying the editing mode {{{2

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
local w; for w in "$@"; do add-zle-hook-widget $w vim-mode-$w; done
} isearch-exit isearch-update line-pre-redraw line-init

typeset -g vim_mode_keymap_state=

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

  # Can be used by prompt themes, etc.
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
# }}}

# Editing mode indicator - Prompt string {{{2

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
          MODE_INDICATOR_VIINS='%F{8}INSERT%f'
          MODE_INDICATOR_VICMD='%F{2}NORMAL%f'
          MODE_INDICATOR_REPLACE='%F{1}REPLACE%f'
          MODE_INDICATOR_SEARCH='%F{5}SEARCH%f'
          MODE_INDICATOR_VISUAL='%F{4}VISUAL%f'
          MODE_INDICATOR_VLINE='%F{4}V-LINE%f'
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

    # Compatibility with oh-my-zsh vi-mode
    function vi_mode_prompt_info() {
      print ${MODE_INDICATOR_PROMPT}
    }

  vim-mode-set-up-indicators
  vim_mode_keymap_funcs+=vim-mode-update-prompt
  # }}}

# }}}

# Finally, make sure the terminal is in application mode, when zle is active. {{{1
# Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
  autoload -Uz add-zle-hook-widget
  function zle_application_mode_start { echoti smkx }
  function zle_application_mode_stop { echoti rmkx }
  add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
  add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi
# }}}

# Restore shell option 'aliases'. This must be the last thing here. {{{1
if [[ $_vim_mode_shopt_aliases = 1 ]]; then
   unset _vim_mode_shopt_aliases
   set -o aliases
fi
# }}}

# vim:fdm=marker:tw=0
