#!/bin/zsh
# zplug - minimalistic plugin manager for ZSH


#
## Globals
#
declare ZPLUG_HOME="${ZPLUG_HOME:-${ZDOTDIR:-$HOME/.config/zsh}/zplug}"
declare ZPLUG_PLUGINS=()
[ -z "$ZPLUG_LOADED_PLUGINS" ] && declare ZPLUG_LOADED_PLUGINS=()



#
## Loggers
#
function __zplug_success() {
  fmt="$1"
  shift 1
  printf "\x1b[32m[ ]\x1b[0m $fmt\n" "$@"
}

function __zplug_warning() {
  fmt="$1"
  shift 1
  printf "\x1b[33m[ ]\x1b[0m $fmt\n" "$@"
}

function __zplug_error() {
  fmt="$1"
  shift 1
  printf "\x1b[31m[ ]\x1b[0m $fmt\n" "$@"
}



#
## Utilities
#
function __zplug_find() {
  local searchdir="$1"
  local searchterm="$2"

  find "$searchdir" -maxdepth 1 -type f -name "$searchterm" | head -n 1
}

function __zplug_check_loaded() {
  local target_plugin="$1" plugin_url

  for plugin_url in ${ZPLUG_LOADED_PLUGINS[*]}; do
    [ "$target_plugin" = "$plugin_url" ] && return
  done

  return 1
}

# Resolve URL shorthand
# user/repo -> https://github.com/user/repo
function __zplug_resolve_url() {
  printf '%s' "$1" | awk -F '/' '{
    if (match($0, /^(git|https?):\/\//)) {
      print $0
    } else {
      print "https://github.com/" $0
    }
  }'
}

# Get last two URL path segments
# https://github.com/user/repo -> user/repo
function __zplug_get_plugin_name() {
  printf '%s' "$1" |\
    awk -F '/' '{ print $(NF - 1) "/" $NF }'
}

function __zplug_mkdir() {
  ! [ -d "$ZPLUG_HOME" ] && \
    mkdir -p "$ZPLUG_HOME" || true
}



#
## Per-plugin tasks
#
function __zplug_install_plugin() {
  local plugin_name clone_url plugin_des

  plugin_name="$(__zplug_get_plugin_name "$1")"
  clone_url="$(__zplug_resolve_url "$1")"
  plugin_dest="$ZPLUG_HOME/$plugin_name"

  # Skip plugin if destination already exists
  # TODO: Maybe add -f option to reinstall such plugins?
  if [ -d "$plugin_dest" ]; then
    __zplug_success '%-50s is already installed, skipping' "$1"
    continue
  fi

  printf 'Installing %s ...\n' "$1"
  git clone "$clone_url" "$plugin_dest" -q --depth 1 || (
    __zplug_error 'Failed to install %-50s, exiting' "$1"
    return 1
  )
}

function __zplug_update_plugin() {
  local plugin_name plugin_dest branch remote diffs

  plugin_name="$(__zplug_get_plugin_name "$1")"
  plugin_dest="$ZPLUG_HOME/$plugin_name"

  git -C "$plugin_dest" remote update >/dev/null

  branch="$(git -C "$plugin_dest" branch --show-current)"
  remote="$(git -C "$plugin_dest" remote show)"

  # If HEAD is detached merge will fail, so will just skip that plugin
  # when a warning. Also this can be used to force some plugins to
  # stay on a single version
  [ -z "$branch" ] && \
    { __zplug_warning '%-50s: HEAD is detached, skipping' "$1" && continue } || true

  # Diff current branch with the remote one to see if there're any
  # updates
  diffs="$(git -C "$plugin_dest" diff "$remote/$branch")"

  if [ -n "$diffs" ]; then
    git -C "$plugin_dest" pull -q "$remote" "$branch" && \
      __zplug_success '%-50s has been successfully updated!' "$1"
  else
    __zplug_success '%-50s is up-to-date!' "$1"
  fi
}

function __zplug_load_plugin() {
  local plugin_name plugin_dest source_zsh_plugin source_dotzsh

  plugin_name="$(__zplug_get_plugin_name "$1")"
  plugin_dest="$ZPLUG_HOME/$plugin_name"

  # Notify user if plugin is not installed yet
  if [ ! -d "$plugin_dest" ]; then
    __zplug_warning '%-50s is not installed,\n\trun `zplug install` to install it' "$1"
    continue
  fi

  # Skip if plugin is already loaded, prevents some plugins from
  # breaking after sourcing them twice
  __zplug_check_loaded "$1" && continue

  # 1st source - .plugin.zsh file
  source_zsh_plugin="$(__zplug_find "$plugin_dest" "*.plugin.zsh")"
  if [ -n "$source_zsh_plugin" ]; then
    source "$source_zsh_plugin"
    ZPLUG_LOADED_PLUGINS+=("$1")
    continue
  fi

  # 2nd source - .zsh file
  source_dotzsh="$(__zplug_find "$plugin_dest" "*.zsh")"
  if [ -n "$source_dotzsh" ]; then
    source "$source_dotzsh"
    ZPLUG_LOADED_PLUGINS+=("$1")
    continue
  fi

  # Throw an error if none of sources has been found so user will know
  # that something went wrong with the downloaded plugin
  __zplug_error 'No .plugin.zsh or .zsh file found, most likely,\n\t%-50s is not a valid ZSH plugin' "$1"
}



#
## zplug
#
function __zplug_usage() {
  cat <<EOF
  zplug - minimalistic plugin manager for ZSH

  Usage:
    zplug <command> [arguments]

  Commands:
    help           -  Show this message
    install        -  Install plugins
    load           -  Source plugins
    plug <source>  -  Register a plugin
    update         -  Update plugins

  About <source>:
    <source> can be either full URL to Git repository or Github's user/repo
    Examples:
      - https://gitlab.com/user/repo
      - zsh-users/repo (expanded to https://github.com/zsh-users/repo)
EOF
}

function __zplug_register() {
  local plugin_url="$1"

  ZPLUG_PLUGINS+=("$plugin_url")
}

function __zplug_install() {
  local plugin_url

  __zplug_mkdir

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    __zplug_install_plugin "$plugin_url"
  done
}

function __zplug_update() {
  local plugin_url

  __zplug_mkdir

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    __zplug_update_plugin "$plugin_url"
  done
}

function __zplug_load() {
  local plugin_url

  __zplug_mkdir

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    __zplug_load_plugin "$plugin_url"
  done
}

#
## Main
#
function zplug() {
  case "$1" in
    plug)
      __zplug_register "$2"
      ;;
    install)
      __zplug_install
      ;;
    update)
      __zplug_update
      ;;
    load)
      __zplug_load
      ;;
    help)
      __zplug_usage
      ;;
    *)
      __zplug_usage
      ;;
  esac
}
