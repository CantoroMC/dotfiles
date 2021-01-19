# System {{{1

# Corrections
alias cp='nocorrect cp'
alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias sudo='nocorrect sudo'

# Useful Lists
alias aliasList="alias | sed 's/=.*//'"
alias funList="declare -f | grep '^[a-z].* ()' | sed 's/{$//'"
alias pathList='echo -e ${PATH//:/\\n}'

# Directory Stack
alias -g ...='../..'
alias -g ....='../../..'

# List directory contents
alias ls='ls --color=auto'
alias l="ls -Ah --sort='extension' --group-directories-first -1"
alias ll="ls -lAh --sort='extension' --group-directories-first --time-style=long-iso"

alias lr="ranger"
alias lc="nnn"

alias t="tree --dirsfirst --si -p -a -L 2 --noreport"
alias td="tree --dirsfirst --si -p -a -L 4 --noreport"
alias ts="tree -p --si --dirsfirst -L 2"

alias L="colorls -1Al --sd"
alias Lg="colorls -1A --git-status --sd"
alias Lt="colorls -A --tree=2 --sd"

# File System Utilities
alias cp="cp -i"
alias diff='diff --color=auto'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias ffd="find . -print | grep -i"

# History
case ${HIST_STAMPS-} in
  "mm/dd/yyyy") alias history='fhistory -f' ;;
  "dd.mm.yyyy") alias history='fhistory -E' ;;
  "yyyy-mm-dd") alias history='fhistory -i' ;;
  "") alias history='fhistory' ;;
  *) alias history="fhistory -t '$HIST_STAMPS'" ;;
esac

# }}}

# Program Alias {{{1

alias abook='abook --config ~/.config/abook/abookrc --datafile ~/Documents/organization/contacts/addressbook'
alias mpm='ncmpcpp'
alias mutt='neomutt'
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'

# Arch
alias pp='pacman'
alias sp="sudo pacman"
# Texmf
alias tlmgr=$TEXMFDIST'/scripts/texlive/tlmgr.pl --usermode'
# Vim
alias vim='nvim'
alias vimS='nvim -S ~/.cache/nvim/sessions/last_session.vim'
alias vimData="cd $XDG_DATA_HOME/nvim"
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
# Haskell Compiler
alias hcompile='ghc --make -outputdir ./out'
# Matlab
alias mat='matlab -nodesktop -nosplash -nojvm'

# }}}

# Frequent files and directories {{{1

# Configuration Files {{{2
# Zsh
alias cfg-zenv="$EDITOR $HOME/.zshenv"
alias cfg-zlog="$EDITOR $HOME/.config/zsh/.zlogin"
alias cfg-alias="$EDITOR $ZDOTDIR/lib/aliases.zsh"
alias cfg-funcs="$EDITOR $ZDOTDIR/lib/functions.zsh"
alias cfg-kbinds="$EDITOR $ZDOTDIR/lib/key-bindings.zsh"
alias cfg-zsh="$EDITOR $ZDOTDIR/.zshrc"
# X
alias cfg-xinit="$EDITOR $XDG_CONFIG_HOME/X11/xinitrc"
alias cfg-xres="$EDITOR $XDG_CONFIG_HOME/X11/xinit/.Xresources"
alias cfg-xprof-dwm="$EDITOR $XDG_CONFIG_HOME/X11/xprofile_dwm"
alias cfg-xprof-xmonad="$EDITOR $XDG_CONFIG_HOME/X11/xprofile_xmonad"

alias cfg-xmon="$EDITOR $XDG_CONFIG_HOME/xmonad/xmonad.hs"
alias cfg-xmob="$EDITOR $XDG_CONFIG_HOME/xmobar/xmobar.hs"

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
alias doMat="cd ~/Documents/programming/MATLAB"
# }}}

# }}}

# vim:fdm=marker:tw=0
