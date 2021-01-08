fpath=($ZDOTDIR/functions/** $fpath)

# Oh My Zsh Configuration {{{1

# ZSH_CUSTOM=~/path/to/files
# Completion
COMPLETION_WAITING_DOTS="true"
ENABLE_CORRECTION="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
# Plugins
plugins=(
  github
  zsh-autosuggestions
  zsh-history-substring-search
  fzf-tab
)
source $ZSH/oh-my-zsh.sh
# }}}

# User Configuration {{{1

zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'

# Configuration files {{{2
for config_file ($ZDOTDIR/lib/*.zsh); do
   source $config_file
done
unset config_file
# }}}

# User Plugins: {{{2

# Fzf
[[ $- == *i* ]] &&
  source "$HOME/.local/share/nvim/plugged/fzf/shell/completion.zsh" 2>/dev/null
source "$HOME/.local/share/nvim/plugged/fzf/shell/key-bindings.zsh"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=12'

user_plugs=(
  timer
)

# Load all of the user-plugins
for user_plug ($user_plugs); do
  if [ -f $ZDOTDIR/plugins/$user_plug/$user_plug.plugin.zsh ]; then
    source $ZDOTDIR/plugins/$user_plug/$user_plug.plugin.zsh
  fi
done
# }}}

# Theme {{{2
autoload -Uz promptinit; promptinit; prompt bull
# }}}

# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

# }}}

# Environmental Variables
export NNN_OPTS="dHUe"
# export NNN_OPENER=nuke
export NNN_BMS='D:~/dotfiles;h:~/Documents/programming/Haskell;x:~/dotfiles/x-window/.config/xmonad'
export NNN_PLUG='i:imgview;p:preview-tui'
# export USE_VIDEOTHUMB='1'
# fzopen;fzcd;vividthumb;dragd
export NNN_COLORS='7342'
export NNN_FCOLORS='c1e2e631c16033f7c6d6abc4'
export NNN_ARCHIVE="\\.(7z|bz2|gz|tar|tgz|zip)$"
export NNN_FIFO='/tmp/nnn.fifo'

# Alias
alias ln='nnn'

# vim:fdm=marker
