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

# Mnemonic: "[G]rep"
alias -g G="|grep"
# Mnemonic: "[W]c"
alias -g W="|wc -l"
# }}}

# Program Alias {{{1
alias abook='abook --config ~/.config/abook/abookrc --datafile ~/Documents/organization/contacts/addressbook'
alias irssi='irssi --config "$XDG_CONFIG_HOME"/irssi/config --home="$XDG_DATA_HOME/irssi"'
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
# Youtube-DL
alias ytpl="youtube-dl -i --extract-audio --audio-format mp3"
alias ytsl="youtube-dl --abort-on-error --no-playlist --extract-audio --audio-format mp3"
# Python
alias py="python"
alias pyFind='find . -name "*.py"'
alias pyGrep='grep -r --include="*.py"'
# OpenFOAM
alias injectOF="[[ -f $FOAM_INST_DIR/OpenFOAM-dev/etc/bashrc ]] && . $FOAM_INST_DIR/OpenFOAM-dev/etc/bashrc"
alias paraFoam='paraFoam -builtin & disown'
# Haskell Compiler
alias hcompile='ghc --make -outputdir ./out'
# Matlab
alias mat='matlab -nodesktop -nosplash -nojvm'
# }}}

# vim:fdm=marker:tw=0
