# History Statistics
function zsh_stats() {
  fc -l 1 \
    | awk '{ CMD[$2]++; count++; } END { for (a in CMD) print CMD[a] " " CMD[a]*100/count "% " a }' \
    | grep -v "./" | sort -nr | head -n20 | column -c3 -s " " -t | nl
}
# History wrapper around fc
function fhistory {
  local clear list
  zparseopts -E c=clear l=list

  if [[ -n "$clear" ]]; then
    # if -c provided, clobber the history file
    echo -n >| "$HISTFILE"
    fc -p "$HISTFILE"
    echo >&2 History file deleted.
  elif [[ -n "$list" ]]; then
    # if -l provided, run as if calling `fc' directly
    builtin fc "$@"
  else
    # unless a number is provided, show all history events (starting from 1)
    [[ ${@[-1]-} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
  fi
}


# Get the value of an alias.
function alias_value() {
    (( $+aliases[$1] )) && echo $aliases[$1]
}
# Try to get the value of an alias, otherwise return the input.
function which_alias() {
    alias_value "$1" || echo "$1"
}

# Append "$1" to "$2"
function append() {
  pcregrep -qM "$1" "$2" || echo "$1" >> "$2"
}

# Set variable "$1" to default value "$2" if "$1" is not yet defined.
#    1. name - The variable to set
#    2. val  - The default value
# Return value:
#    0 if the variable exists, 3 if it was set
function set_default() {
    (( $+parameters[$1] )) && return 0
    typeset -g "$1"="$2"   && return 3
}

# Set environment variable "$1" to default value "$2" if "$1" is not yet defined.
# Arguments:
#    1. name - The env variable to set
#    2. val  - The default value
# Return value:
#    0 if the env variable exists, 3 if it was set
function env_default() {
    [[ ${parameters[$1]} = *-export* ]] && return 0
    export "$1=$2" && return 3
}


# Directory Stack
function d() {
  if [[ -n $1 ]]; then
    dirs "$@"
  else
    dirs -v | head -10
  fi
}
compdef _dirs d

# ranger
function rcd() {
  # Allow to change directory using ranger
  ranger --choosedir=$XDG_CACHE_HOME/ranger_dir
  dir=$(cat $XDG_CACHE_HOME/ranger_dir)
  [ -n "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
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
    exit 1
  elif [ "$1" = -h ] || [ "$1" = --help ]; then
    lsg_help
    exit 0
  else
    keyword=$(printf "%s\n" "${@/ /.*}")
    ls -hlA --color=yes \
      | awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)*2^(8-i));if(k)printf(" %0o ",k);print}' \
      | grep -iE "$keyword"
  fi
}

# open and disown it
function open() {
  xdg-open $1 & disown
}

# Rename all the files in a folder with numbered sequence
function bulk_rename() { # Usage: functionName Folder Name
  [ "${#}" -eq 1 ] && 
    {
      local name="${1}"
      ls -v | cat -n | while read n f; do mv -n "$f" "${name}$n.${f##*.}"; done;
    }
}


# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
typeset -AH FX FG BG

FX=(
  reset     "%{[00m%}"
  bold      "%{[01m%}" no-bold      "%{[22m%}"
  italic    "%{[03m%}" no-italic    "%{[23m%}"
  underline "%{[04m%}" no-underline "%{[24m%}"
  blink     "%{[05m%}" no-blink     "%{[25m%}"
  reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
  FG[$color]="%{[38;5;${color}m%}"
  BG[$color]="%{[48;5;${color}m%}"
done

# Show all 256 colors with color number
function spectrum_ls() {
  local ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Supercalifragilisticexpialidocious}
  for code in {000..255}; do
    print -P -- "$code: $FG[$code]$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
function spectrum_bls() {
  local ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}
  for code in {000..255}; do
    print -P -- "$code: $BG[$code]$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}


zmodload zsh/langinfo  # Required for $langinfo
# URL-encode a string
#
# Encodes a string using RFC 2396 URL-encoding (%-escaped).
# See: https://www.ietf.org/rfc/rfc2396.txt
#
# By default, reserved characters and unreserved "mark" characters are
# not escaped by this function. This allows the common usage of passing
# an entire URL in, and encoding just special characters in it, with
# the expectation that reserved and mark characters are used appropriately.
# The -r and -m options turn on escaping of the reserved and mark characters,
# respectively, which allows arbitrary strings to be fully escaped for
# embedding inside URLs, where reserved characters might be misinterpreted.
#
# Prints the encoded string on stdout.
# Returns nonzero if encoding failed.
#
# Usage:
#  urlencode [-r] [-m] [-P] <string>
#
#    -r causes reserved characters (;/?:@&=+$,) to be escaped
#    -m causes "mark" characters (_.!~*''()-) to be escaped
#    -P causes spaces to be encoded as '%20' instead of '+'
function urlencode() {
  emulate -L zsh
  local -a opts
  zparseopts -D -E -a opts r m P

  local in_str=$1
  local url_str=""
  local spaces_as_plus
  if [[ -z $opts[(r)-P] ]]; then spaces_as_plus=1; fi
  local str="$in_str"

  # URLs must use UTF-8 encoding; convert str to UTF-8 if required
  local encoding=$langinfo[CODESET]
  local safe_encodings
  safe_encodings=(UTF-8 utf8 US-ASCII)
  if [[ -z ${safe_encodings[(r)$encoding]} ]]; then
    str=$(echo -E "$str" | iconv -f $encoding -t UTF-8)
    if [[ $? != 0 ]]; then
      echo "Error converting string from $encoding to UTF-8" >&2
      return 1
    fi
  fi

  # Use LC_CTYPE=C to process text byte-by-byte
  local i byte ord LC_ALL=C
  export LC_ALL
  local reserved=';/?:@&=+$,'
  local mark='_.!~*''()-'
  local dont_escape="[A-Za-z0-9"
  if [[ -z $opts[(r)-r] ]]; then
    dont_escape+=$reserved
  fi
  # $mark must be last because of the "-"
  if [[ -z $opts[(r)-m] ]]; then
    dont_escape+=$mark
  fi
  dont_escape+="]"

  # Implemented to use a single printf call and avoid subshells in the loop,
  # for performance (primarily on Windows).
  local url_str=""
  for (( i = 1; i <= ${#str}; ++i )); do
    byte="$str[i]"
    if [[ "$byte" =~ "$dont_escape" ]]; then
      url_str+="$byte"
    else
      if [[ "$byte" == " " && -n $spaces_as_plus ]]; then
        url_str+="+"
      else
        ord=$(( [##16] #byte ))
        url_str+="%$ord"
      fi
    fi
  done
  echo -E "$url_str"
}

# URL-decode a string
#
# Decodes a RFC 2396 URL-encoded (%-escaped) string.
# This decodes the '+' and '%' escapes in the input string, and leaves
# other characters unchanged. Does not enforce that the input is a
# valid URL-encoded string. This is a convenience to allow callers to
# pass in a full URL or similar strings and decode them for human
# presentation.
#
# Outputs the encoded string on stdout.
# Returns nonzero if encoding failed.
#
# Usage:
#   urldecode <urlstring>  - prints decoded string followed by a newline
function urldecode() {
  emulate -L zsh
  local encoded_url=$1

  # Work bytewise, since URLs escape UTF-8 octets
  local caller_encoding=$langinfo[CODESET]
  local LC_ALL=C
  export LC_ALL

  # Change + back to ' '
  local tmp=${encoded_url:gs/+/ /}
  # Protect other escapes to pass through the printf unchanged
  tmp=${tmp:gs/\\/\\\\/}
  # Handle %-escapes by turning them into `\xXX` printf escapes
  tmp=${tmp:gs/%/\\x/}
  local decoded
  eval "decoded=\$'$tmp'"

  # Now we have a UTF-8 encoded string in the variable. We need to re-encode
  # it if caller is in a non-UTF-8 locale.
  local safe_encodings
  safe_encodings=(UTF-8 utf8 US-ASCII)
  if [[ -z ${safe_encodings[(r)$caller_encoding]} ]]; then
    decoded=$(echo -E "$decoded" | iconv -f UTF-8 -t $caller_encoding)
    if [[ $? != 0 ]]; then
      echo "Error converting string from UTF-8 to $caller_encoding" >&2
      return 1
    fi
  fi

  echo -E "$decoded"
}

# Dotfiles Encryption
function en-dot() {
  cd ~/dotfiles
  tar czf private.tar.gz private
  gpg -er marco.cantoro92@outlook.it private.tar.gz
  rm private.tar.gz
}

function de-dot() {
  cd ~/dotfiles
  gpg -do private.tar.gz private.tar.gz.gpg
  tar xvf private.tar.gz
  rm private.tar.gz
}

# System clipboard integration
function detect-clipboard() {
  emulate -L zsh

  # clipcopy - Copy data to clipboard
  # Usage:
  #  <command> | clipcopy    - copies stdin to clipboard
  #  clipcopy <file>         - copies a file's contents to clipboard

  # clippaste - "Paste" data from clipboard to stdout
  # Usage:
  #   clippaste              - writes clipboard's contents to stdout
  #   clippaste | <command>  - pastes contents and pipes it to another process
  #   clippaste > <file>     - paste contents to a file

  if [ -n "${DISPLAY:-}" ] && (( ${+commands[xclip]} )); then
    function clipcopy() { xclip -in -selection clipboard < "${1:-/dev/stdin}"; }
    function clippaste() { xclip -out -selection clipboard; }
  elif [ -n "${DISPLAY:-}" ] && (( ${+commands[xsel]} )); then
    function clipcopy() { xsel --clipboard --input < "${1:-/dev/stdin}"; }
    function clippaste() { xsel --clipboard --output; }
  elif [ -n "${TMUX:-}" ] && (( ${+commands[tmux]} )); then
    function clipcopy() { tmux load-buffer "${1:--}"; }
    function clippaste() { tmux save-buffer -; }
  else
    function _retry_clipboard_detection_or_fail() {
      local clipcmd="${1}"; shift
      if detect-clipboard; then
        "${clipcmd}" "$@"
      else
        print "${clipcmd}: Platform $OSTYPE not supported or xclip/xsel not installed" >&2
        return 1
      fi
    }
    function clipcopy() { _retry_clipboard_detection_or_fail clipcopy "$@"; }
    function clippaste() { _retry_clipboard_detection_or_fail clippaste "$@"; }
    return 1
  fi
}
detect-clipboard || true


function command_not_found_handler() {
  # command-not-found handler will use pacman directly to search for matching
  # packages when an unknown command is executed
  local pkgs cmd="$1" files=()
  files=(${(f)"$(pacman -F --machinereadable -- "/usr/bin/${cmd}")"})
  if (( ${#files[@]} )); then
    printf '%s may be found in the following packages:\n' "$cmd"
    local res=() repo package version file
    for file in "$files[@]"; do
      res=("${(0)file}")
      repo="$res[1]"
      package="$res[2]"
      version="$res[3]"
      file="$res[4]"
      printf '  %s/%s %s: /%s\n' "$repo" "$package" "$version" "$file"
    done
  else
    printf 'zsh: command not found: %s\n' "$cmd"
  fi
  return 127
}

# extractor - archive extractor (usage: extractor <archive>)
function extractor() {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2|*.tbz2)   tar xjf $1 ;;
      *.tar.gz|*.tgz)     tar xzf $1 ;;
      *.tar.xz)           tar -xf $1 ;;
      *.tar)              tar xf $1 ;;
      *.bz2)              bunzip2 $1 ;;
      *.rar)              unrar x -ad $1 ;;
      *.gz)               gunzip $1 ;;
      *.Z)                uncompress $1 ;;
      *.7z|*.zip)         7z x $1 ;;
      *.xz)               unxz $1 ;;
      *)
        tput setaf 1
        echo "Extracter: $1 - unknown archive method"
        tput sgr0;;
    esac
  else
    printf "File \"%s\" is not supported.\\n" $1
  fi
}

function fast-git() {
  git add -A
  [ "$#" -eq 1 ] && git commit -m "$1"
  [ "$#" -eq 0 ] && git commit
  git push
}

function ls-git() {
  find "${1:-.}" -type d -name .git -print |
    sed -e s,/.git$,, |
    xargs -r \
    du -sh |
    sort -k2
}

function pyclean() {
  PYCLEAN_DIR=${*:-'.'}
  find ${PYCLEAN_DIR} -type f -name "*.py[co]" -delete
  find ${PYCLEAN_DIR} -type d -name "__pycache__" -delete
  find ${PYCLEAN_DIR} -depth -type d -name ".mypy_cache" -exec rm -r "{}" +
  find ${PYCLEAN_DIR} -depth -type d -name ".pytest_cache" -exec rm -r "{}" +
}

# Weather
function meteo() {
  local LOCALE=$(echo ${LANG:-en} | cut -c1-2)
  if [ $# -eq 0 ]; then
    local LOCATION=$(curl -s ipinfo.io/loc)
  else
    local LOCATION=$1
  fi
  curl -s "$LOCALE.wttr.in/$LOCATION?qF"
}
# Moon
function moon() {
  curl -s "wttr.in/moon?F"
}

# Videos collage
function imageFromVideo() {
  VID_NAME=${2%.*}
  case ${1} in
    "-s" | "--single")
      ffmpegthumbnailer -i "${2}" -o "./${VID_NAME}.jpg" -s 0 -q 10 -t ${3};;
    "-m" | "--mutliple")
      for ii in $(seq 0 ${3} 100); do
        ffmpegthumbnailer -i "${2}" -o "./${VID_NAME}$ii.png" -s 0 -q 10 -t "${ii}"
      done
      ;;
    *)
      echo -e "\e[1;34mimageFromVideo -s Video Time\e[0m for taking a picture of Video at the time"
      echo -e "       specified as absolute time hh:mm:ss or as percentage."
      echo -e "\e[1;34mimageFromVideo -m Video Snaps\e[0m for taking pictures of Video at regular"
      echo -e "       intervals every Snaps percentage."
      ;;
  esac
}

function thumbCollage() {
  VID_NAME=${1%.*}
  montage ${VID_NAME}*.png -mode Concatenate -tile 4x4 -geometry +0+0 ${VID_NAME}.jpg
  rm ${VID_NAME}*.png
}
