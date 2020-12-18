command_not_found_handler() {
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
extractor() {
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

# open and disown it
open() {
  xdg-open $1 & disown
}

pyclean() {
  PYCLEAN_DIR=${*:-'.'}
  find ${PYCLEAN_DIR} -type f -name "*.py[co]" -delete
  find ${PYCLEAN_DIR} -type d -name "__pycache__" -delete
  find ${PYCLEAN_DIR} -depth -type d -name ".mypy_cache" -exec rm -r "{}" +
  find ${PYCLEAN_DIR} -depth -type d -name ".pytest_cache" -exec rm -r "{}" +
}

# ranger navigation
rcd() {
  # Allow to change directory using ranger
  ranger --choosedir=$XDG_CACHE_HOME/ranger_dir
  dir=$(cat $XDG_CACHE_HOME/ranger_dir)
  [ -n "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
}

append() {
  pcregrep -qM "$1" "$2" || echo "$1" >> "$2"
}

fast-git(){
  git add -A
  [ "$#" -eq 1 ] && git commit -m "$1"
  [ "$#" -eq 0 ] && git commit
  git push
}

# Encryption
en-dot() {
  cd ~/dotfiles
  tar czf private.tar.gz private
  gpg -er marco.cantoro92@outlook.it private.tar.gz
  rm private.tar.gz
}
de-dot() {
  cd ~/dotfiles
  gpg -do private.tar.gz private.tar.gz.gpg
  tar xvf private.tar.gz
  rm private.tar.gz
}

# Weather
meteo() {
  local LOCALE=$(echo ${LANG:-en} | cut -c1-2)
  if [ $# -eq 0 ]; then
    local LOCATION=$(curl -s ipinfo.io/loc)
  else
    local LOCATION=$1
  fi
  curl -s "$LOCALE.wttr.in/$LOCATION?qF"
}
# Moon
moon() {
  curl -s "wttr.in/moon?F"
}

# Rename all the files in a folder with numbered sequence
bulk_rename() { # Usage: functionName Folder Name
  [ "${#}" -eq 1 ] && 
    {
      local name="${1}"
      ls -v | cat -n | while read n f; do mv -n "$f" "${name}$n.${f##*.}"; done;
    }
}

imageFromVideo() {
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

thumbCollage() {
  VID_NAME=${1%.*}
  montage ${VID_NAME}*.png -mode Concatenate -tile 4x4 -geometry +0+0 ${VID_NAME}.jpg
  rm ${VID_NAME}*.png
}
