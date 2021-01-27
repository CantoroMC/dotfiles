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

_nnn_cmd () {
  BUFFER="nnn"
  zle end-of-line
  zle accept-line
}
zle -N _nnn_cmd

_tmux_cmd () {
  BUFFER="tmux"
  zle end-of-line
  zle accept-line
}
zle -N _tmux_cmd

_nvim_cmd () {
  BUFFER="nvim"
  zle end-of-line
  zle accept-line
}
zle -N _nvim_cmd
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

# Additional binding for Delete
bindkey -M viins "^[[3~" delete-char
bindkey -M vicmd "^[[3~" delete-char
bindkey -M viins "^[[P" delete-char
bindkey -M vicmd "^[[P" delete-char
bindkey -M viins "^[3;5~" delete-char
bindkey -M vicmd "^[3;5~" delete-char

# }}}

# Ctrl+Key {{{2
bindkey -M viins    '^A'    _tmux_cmd
bindkey -M viins    '^B'    exchange-point-and-mark
bindkey -M viins    '^E'    expand-cmd-path
bindkey -M viins    '^F'    accept-and-infer-next-history
bindkey -M viins    '^K'    vi-quoted-insert
bindkey -M viins    '^G'    menu-complete
# bindkey -M viins    '^I'    fzf-tab-complete
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
bindkey -M viins    '^X^L' _nnn_cmd
bindkey -M vicmd    '^X^L' _nnn_cmd
bindkey -M viins    '^X^E' _nvim_cmd
bindkey -M vicmd    '^X^E' _nvim_cmd
bindkey -M viins -s '^X^M' 'fzf-man\n'
bindkey -M vicmd -s '^X^M' 'fzf-man\n'
bindkey -M viins -s '^X^F' 'rcd\n'
bindkey -M vicmd -s '^X^F' 'rcd\n'
bindkey -M viins -s '^X^N' 'rcn\n'
bindkey -M vicmd -s '^X^N' 'rcn\n'
# }}}

# Alt-Key {{{2
# bindkey -M viins    '\ec'  fzf-cd-widget
bindkey -M viins    '\ea'  beginning-of-line
bindkey -M viins    '\eb'  backward-word
bindkey -M viins    '\ee'  end-of-line
bindkey -M viins    '\ef'  forward-word
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

# MenuSelect Map {{{2
bindkey -M menuselect '^o' accept-and-infer-next-history
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
