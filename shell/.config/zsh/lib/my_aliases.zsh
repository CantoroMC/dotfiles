# System {{{1
alias aliasList="alias | sed 's/=.*//'"
alias funList="declare -f | grep '^[a-z].* ()' | sed 's/{$//'"
alias pathList='echo -e ${PATH//:/\\n}'
# Directories
alias cp="cp -i"
alias ffd="find . -print | grep -i"
alias L="colorls -1Al --sd"
alias Lg="colorls -1A --git-status --sd"
alias Lt="colorls -A --tree=2 --sd"
alias lr="ranger"
alias lse="ls -lA --sort='extension' --group-directories-first --time-style=long-iso"
alias lsc="ls -A --sort='extension' --group-directories-first -1"
alias lss="dirs -v | head -10"
alias t="tree --dirsfirst --si -p -a -L 2 --noreport"
alias td="tree --dirsfirst --si -p -a -L 4 --noreport"
alias ts="tree -p --si --dirsfirst -L 2"
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
# }}}

# Program Alias {{{1
alias abook='abook --config ~/.config/abook/abookrc --datafile ~/Documents/organization/contacts/addressbook'
alias mat='matlab -nodesktop -nosplash'
alias mpm='ncmpcpp'
alias mutt='neomutt'
# Git
alias gitu='git add . && git commit && git push'
# Arch
alias sp="sudo pacman"
# Haskell
# Texmf
alias tlmgr=$TEXMFDIST'/scripts/texlive/tlmgr.pl --usermode'
# Vim
alias vim='nvim'
alias vimS='nvim -S ~/.cache/nvim/sessions/last_session.vim'
alias vimData="cd $XDG_DATA_HOME/nvim"
alias wiki="$EDITOR $XDG_DATA_HOME/nvim/wiki/index.md"
# Youtube-DL
alias ytpl="youtube-dl -i --extract-audio --audio-format mp3"
alias ytsl="youtube-dl --abort-on-error --no-playlist --extract-audio --audio-format mp3"
# Python
alias py="python"
alias pyFind='find . -name "*.py"'
alias pyGrep='grep -r --include="*.py"'
# OpenFOAM
alias injectOF="[[ -f $FOAM_INST_DIR/OpenFOAM-8/etc/bashrc ]] && . $FOAM_INST_DIR/OpenFOAM-8/etc/bashrc"
alias paraFoam='paraFoam -builtin & disown'
# }}}

# Frequent files and directories {{{1

# Configuration Files {{{2
# Zsh
alias cfg-zenv="$EDITOR $HOME/.zshenv"
alias cfg-alias="$EDITOR $ZDOTDIR/lib/my_aliases.zsh"
alias cfg-funcs="$EDITOR $ZDOTDIR/lib/my_functions.zsh"
alias cfg-kbinds="$EDITOR $ZDOTDIR/lib/key-bindings.zsh"
alias cfg-zsh="$EDITOR $ZDOTDIR/.zshrc"
# X
alias cfg-xinit="$EDITOR $XDG_CONFIG_HOME/X11/xinitrc"
alias cfg-xres="$EDITOR $XDG_CONFIG_HOME/X11/xinit/.Xresources"
alias cfg-xprof="$EDITOR $XDG_CONFIG_HOME/X11/xprofile"
alias cfg-xerr="$EDITOR $HOME/.xsession-errors"

alias cfg-xmon="$EDITOR $XDG_CONFIG_HOME/xmonad/xmonad.hs"
alias cfg-xmobtop="$EDITOR $XDG_CONFIG_HOME/xmobar/xmobar.hs"

# Everything else
alias cfg-vim="$EDITOR $XDG_CONFIG_HOME/nvim/init.vim"
alias cfg-git="$EDITOR $XDG_CONFIG_HOME/git/config"
alias cfg-tmux="$EDITOR $XDG_CONFIG_HOME/tmux/tmux.conf"
alias cfg-rofi="$EDITOR $XDG_CONFIG_HOME/rofi/config.rasi"
# }}}

# Frequent folder: {{{2
alias thesis="cd ~/Documents/programming/TeX/matriHX"
alias curriculum="cd ~/Documents/programming/TeX/curriculum"
alias doHs="cd ~/Documents/programming/Haskell"
# }}}

# }}}

# vim:fdm=marker:tw=0
