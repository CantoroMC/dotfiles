zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'exa --color=always -aT -L=2 --group-directories-first -I=.git $realpath'
zstyle ':fzf-tab:*' switch-group '[' ']'
zstyle ':fzf-tab:complete:cd:*' popup-pad 20 0
