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

# Native Widgets {{{2
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N down-line-or-beginning-search
zle -N up-line-or-beginning-search

autoload -Uz edit-command-line
zle -N edit-command-line
# }}}

# Fzf widgets {{{2
autoload fzf-git fzf-bindkey fzf-kill fzf-bmarks fzf-configs

zle -N fzf-git-branches fzf-git
zle -N fzf-git-files fzf-git
zle -N fzf-git-hashes fzf-git
zle -N fzf-git-remotes fzf-git
zle -N fzf-git-tags fzf-git
zle -N fzf-bindkey
zle -N fzf-kill
zle -N fzf-bmarks
zle -N fzf-configs
# }}}

# Most used commands -> Widgets: {{{2
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
bindkey -M viins      '^A'    _tmux_cmd
bindkey -M viins      '^B'    exchange-point-and-mark
bindkey -M viins      '^E'    expand-cmd-path
bindkey -M viins      '^F'    accept-and-infer-next-history
bindkey -M viins      '^K'    vi-quoted-insert
bindkey -M viins      '^G'    menu-complete
# bindkey -M viins      '^I'    fzf-tab-complete
bindkey -M viins      '^L'    clear-screen
bindkey -M viins      '^N'    history-substring-search-down
bindkey -M menuselect '^O'    accept-and-infer-next-history
bindkey -M viins      '^P'    history-substring-search-up
bindkey -M viins      '^Q'    _exit-cmd
bindkey -M vicmd      '^Q'    _exit-cmd
# bindkey -M viins      '^R'    fzf-history-widget
# bindkey -M viins      '^T'    fzf-file-widget
bindkey -M viins      '^V'    edit-command-line
bindkey -M vicmd      '^V'    edit-command-line
bindkey -M viins      '^Y'    copy-prev-shell-word
bindkey -M viins      ' '     magic-space

bindkey -M viins      '^G^B' fzf-git-branches
bindkey -M viins      '^G^F' fzf-git-files
bindkey -M viins      '^G^H' fzf-git-hashes
bindkey -M viins      '^G^R' fzf-git-remotes
bindkey -M viins      '^G^T' fzf-git-tags
bindkey -M viins -s   '^X^F' 'rcn\n'
bindkey -M viins      '^X^K' fzf-bindkey
bindkey -M viins      '^X^L' _nnn_cmd
bindkey -M viins -s   '^X^M' 'fzf-man\n'
bindkey -M viins      '^X^N' _nvim_cmd
bindkey -M viins      '^X^P' fzf-kill
bindkey -M viins      '^X^S' fzf-bmarks
bindkey -M viins      '^X^Z' fzf-configs
# }}}

# Alt-Key: Emacs Like {{{2
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
