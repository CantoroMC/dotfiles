#
# Aliases
#
# Directory Stack
alias -g ...='../..'
alias -g ....='../../..'
alias git-root='cd $(git rev-parse --show-cdup)'
# List directory contents
alias ls='ls --color=auto'
alias l="ls -Ah --sort='extension' --group-directories-first -1"
alias ll="ls -lAh --sort='extension' --group-directories-first --time-style=long-iso"
alias lc="nnn"
alias t="tree --dirsfirst --si -p -a -L 2 --noreport -I .git"
alias td="tree --dirsfirst --si -p -a -L 4 --noreport -I .git"
alias ts="tree -p --si --dirsfirst -L 2 -I .git"
alias L="exa -alg --group-directories-first --icons --git"
alias Ls="exa -a1 --group-directories-first --icons"
alias Lt="exa -algT -L=3 --group-directories-first --icons --git -I=.git"
alias z="cdr"

#
# Functions
#
# Directory Stack
function d() {
  if [[ -n $1 ]]; then
    dirs "$@"
  else
    dirs -v | head -10
  fi
}
compdef _dirs d

# Short for ../../
.{1..9} (){
  local d=.
  repeat ${0:1} d+=/..
  cd $d
}

# nnn
function rcn() {
  # Block nesting of nnn in subshells
  if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
    echo "nnn is already running"
    return
  fi

  export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

  nnn "$@"

  if [ -f "$NNN_TMPFILE" ]; then
    . "$NNN_TMPFILE"
    rm -f "$NNN_TMPFILE" > /dev/null
  fi
}

function lsg_help() {
  printf "%s\n" "desc: list and grep with file permission and color highlights"
  printf "%s\n" "dependency: grep awk"
  printf "\n"
  printf "%s\n" "usage: ${0##*/} [keyword]"
  printf "\n"
  printf "%s\n" "  $ ${0##*/} png"
}

function lsg() {
  if [ $# -lt 1 ]; then
    lsg_help
  elif [ "$1" = -h ] || [ "$1" = --help ]; then
    lsg_help
  else
    keyword=$(printf "%s\n" "${@/ /.*}")
    ls -hlA --color=yes \
      | awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)*2^(8-i));if(k)printf(" %0o ",k);print}' \
      | grep -iE "$keyword"
  fi
}

#
# Remembering recent directories with `cdr`
#
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# enable menu-style completion for cdr
zstyle ':completion:*:*:cdr:*:*' menu selection
# insert directly the directory path and not the index
zstyle ':completion:*:*:cdr:*:*' recent-dirs-insert true
# fall through to cd if cdr is passed a non-recent dir as an argument
zstyle ':chpwd:*' recent-dirs-default true
# maximum number of nodes to save to the $ZDOTDIR/.chpwd-recent-dirs
zstyle ':chpwd:*' recent-dirs-max 100
# directories that sould not be added to the recent list
zstyle ':chpwd:*' recent-dirs-prune parent

#
# Automatically ls after cd
#
function -auto-ls-after-cd() {
  emulate -L zsh
  # Only in response to a user-initiated `cd`, not indirectly (eg. via another
  # function).
  if [ "$ZSH_EVAL_CONTEXT" = "toplevel:shfunc" ]; then
    ls -A
  fi
}
add-zsh-hook chpwd -auto-ls-after-cd
