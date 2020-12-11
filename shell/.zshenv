# XDG based directories {{{1
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-${HOME}/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}"
export XDG_CONFIG_DIRS="${XDG_CONFIG_DIRS:-/etc/xdg}"
# }}}

# User Variables {{{1
export DOTFILES="$HOME"/dotfiles

export ADDRESSES="$HOME/Documents/organization/contacts/addressbook"
export ARCHFLAGS="-arch x86_64"
export EMAIL='marco.cantoro92@outlook.it'
export NAME='Marco Cantoro'

# Color man pages: {{{2
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r
# }}}

# Password Helper
if whence $HOME/.local/bin/dmenu_askpass &>/dev/null; then
  export SUDO_ASKPASS="$HOME/.local/bin/dmenu_askpass"
fi

# }}}

# ZSH variables: {{{1

export ZDOTDIR="$HOME/.config/zsh"
export HISTFILE=$XDG_CACHE_HOME/zsh_history

# Oh My Zsh Variable
export ZSH="$XDG_CONFIG_HOME/zsh/oh-my-zsh"

# ZLS_COLORS and LS_COLORS: {{{2

# ZLS_COLORS: {{{3
ZLS_COLORS='no=00:fi=00:rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32'
# Archives or compressed files (bright red)
ZLS_COLORS=${ZLS_COLORS}':*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31'
# Image formats (bright magenta)
ZLS_COLORS=${ZLS_COLORS}':*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35'
ZLS_COLORS=${ZLS_COLORS}':*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35'
# Document formats (green)
ZLS_COLORS=${ZLS_COLORS}':*.pdf=00;32:*.ps=00;32:*.txt=00;32:*.patch=00;32:*.diff=00;32:*.log=00;32:*.doc=00;32'
# Audio formats (cyan)
ZLS_COLORS=${ZLS_COLORS}':*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36'
# Additional coloring
ZLS_COLORS=${ZLS_COLORS}':*.c=01;39:*.cpp=01;39:*.py=01;39:*.md=00;34:*.vim=01;31:*.tex=01;33:*.bib=01;33:*.sty=01;33:*.cls=01;33:';
# }}}

LS_COLORS=${ZLS_COLORS}
export ZLS_COLORS LS_COLORS

# }}}

# }}}

# Language/Programs Specifics {{{1

# OpenFOAM
export FOAM_INST_DIR='/usr/local/OpenFOAM'

# TexMf Trees and Kpathsea
export TEXMFDIST="/usr/share/texmf-dist"
export TEXMFLOCAL="/usr/share/texmf"
export TEXMFSYSVAR="/var/lib/texmf"
export TEXMFSYSCONFIG="/etc/texmf/"
export TEXMFHOME="$XDG_DATA_HOME/texmf"
export TEXMFVAR="$XDG_CONFIG_HOME/texlive/texmf-var"
export TEXMFCONFIG="$XDG_CONFIG_HOME/texlive/texmf-config"

# Ranger
export RANGER_LOAD_DEFAULT_RC="FALSE"

# Nix Package Manager
[ -e /home/cantoro/.nix-profile/etc/profile.d/nix.sh ] && . /home/cantoro/.nix-profile/etc/profile.d/nix.sh
# }}}

# Path {{{1

typeset -U PATH path

for bin_dir ($HOME/.local/bin/*/); do
  bin=$(echo "${bin_dir}" | sed 's/.$//')
  path=("${bin}" "$path[@]")
done
unset bin_dir

path=(
  "$GOPATH/bin"
  "$GEM_HOME/bin"
  "$XDG_DATA_HOME/nvim/plugged/fzf/bin"
  "$path[@]"
)

export PATH

# }}}

# Fuzzy Finder {{{1

if whence fzf &>/dev/null; then
  export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
  export FZF_CTRL_T_COMMAND='rg --hidden -l ""'
  export FZF_DEFAULT_OPTS='
    --height=50%
    --min-height=15
    --ansi
    --layout=reverse
    --multi --info=inline
    --color=fg:#B8CC52,bg:#151a1e,hl:#B8CC52,fg+:#95E6CB,bg+:#000000,hl+:#fff779,info:#f29718,prompt:#36A3D9,spinner:#fff779,pointer:#FF3333,border:#A37ACC,marker:#B8CC52,header:#B8CC52,preview-fg:#FFFFFF
    '
fi

# Bibtex
# FZF_BIBTEX_SOURCES: path to bibtex file; multiple items separated by a ':'
export FZF_BIBTEX_SOURCES="$TEXMFHOME/bibtex/bib/matriHX.bib"

# }}}

# vim:fdm=marker
