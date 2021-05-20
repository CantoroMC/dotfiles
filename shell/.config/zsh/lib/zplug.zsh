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
  printf "\x1b[32m$fmt\x1b[0m\n" "$@"
}
function __zplug_warning() {
  fmt="$1"
  shift 1
  printf "[warn] \x1b[33m$fmt\x1b[0m\n" "$@"
}
function __zplug_error() {
  fmt="$1"
  shift 1
  printf "[err] \x1b[31m$fmt\x1b[0m\n" "$@"
}


#
## Utilities
#

# Friendly wrapper around find
function __zplug_find() {
  local searchdir="$1"
  local searchterm="$2"

  find "$searchdir" -maxdepth 1 -type f -name "$searchterm" | head -n 1
}

# Check if plugin is already loaded
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
  printf '%s' "$1" | awk -F '/' '{ print $(NF - 1) "/" $NF }'
}


#
## zplug
#

# Show help message
function __zplug_usage() {
  cat <<EOF
zplug - minimalistic plugin manager for ZSH
Usage: zplug <command> [arguments]
Commands:
  plugin <source> - Register a plugin
  install - Install plugins
  update - Update plugins
  help - Show this message
About <source>:
  <source> can be either full URL to Git repository or Github's user/repo
  Examples: https://gitlab.com/user/repo, zsh-users/repo (expanded to https://github.com/zsh-users/repo)
EOF
}

# Register a plugin
function __zplug_plugin() {
  local plugin_url="$1"

  ZPLUG_PLUGINS+=("$plugin_url")
}

# Install plugins
function __zplug_install() {
  local plugin_url plugin_name clone_url clone_dest

  # Make sure ZPLUG_HOME exists
  mkdir -p "$ZPLUG_HOME"

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    plugin_name="$(__zplug_get_plugin_name "$plugin_url")"

    # Because URL is not always full, we need to resolve it first
    clone_url="$(__zplug_resolve_url "$plugin_url")"
    clone_dest="$ZPLUG_HOME/$plugin_name"

    # Skip plugin if destination already exists
    # TODO: Maybe add -f option to reinstall such plugins?
    if [ -d "$clone_dest" ]; then
      __zplug_warning '%s is already installed, skipping' "$plugin_url"
      continue
    fi

    printf 'Installing %s ...\n' "$plugin_url"
    git clone "$clone_url" "$clone_dest" -q --depth 1 || (
      __zplug_error 'Failed to install %s, exiting' "$plugin_url"
      return 1
    )
  done
}

# Update plugins
function __zplug_update() {
  local plugin_url plugin_name plugin_location branch remote diffs

  # Make sure ZPLUG_HOME exists
  mkdir -p "$ZPLUG_HOME"

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    plugin_name="$(__zplug_get_plugin_name "$plugin_url")"
    plugin_location="$ZPLUG_HOME/$plugin_name"

    git -C "$plugin_location" remote update >/dev/null

    branch="$(git -C "$plugin_location" branch --show-current)"
    remote="$(git -C "$plugin_location" remote show)"

    # If HEAD is detached merge will fail, so will just skip that plugin
    # when a warning. Also this can be used to force some plugins to
    # stay on a single version
    [ -z "$branch" ] && __zplug_warning '%s: HEAD is detached, skipping' "$plugin_url" && continue

    # Diff current branch with the remote one to see if there're any
    # updates
    diffs="$(git -C "$plugin_location" diff "$remote/$branch")"

    if [ -n "$diffs" ]; then
      git -C "$plugin_location" pull -q "$remote" "$branch" && __zplug_success '%s has been successfully updated!' "$plugin_url"
    else
      __zplug_warning '%s is up-to-date!' "$plugin_url"
    fi
  done
}

# Load plugins
function __zplug_load() {
  local plugin_url plugin_name plugin_location source_zsh_plugin source_dotzsh

  # Make sure ZPLUG_HOME exists
  mkdir -p "$ZPLUG_HOME"

  for plugin_url in ${ZPLUG_PLUGINS[*]}; do
    plugin_name="$(__zplug_get_plugin_name "$plugin_url")"
    plugin_location="$ZPLUG_HOME/$plugin_name"

    # Notify user if plugin is not installed yet
    if [ ! -d "$plugin_location" ]; then
      __zplug_warning '%s is not installed, run `zplug install` to install it' "$plugin_url"
      continue
    fi

    # Skip if plugin is already loaded, prevents some plugins from
    # breaking after sourcing them twice
    __zplug_check_loaded "$plugin_url" && continue

    # 1st source - .plugin.zsh file
    source_zsh_plugin="$(__zplug_find "$plugin_location" "*.plugin.zsh")"

    if [ -n "$source_zsh_plugin" ]; then
      source "$source_zsh_plugin"
      ZPLUG_LOADED_PLUGINS+=("$plugin_url")

      continue
    fi

    # 2nd source - .zsh file
    source_dotzsh="$(__zplug_find "$plugin_location" "*.zsh")"

    if [ -n "$source_dotzsh" ]; then
      source "$source_dotzsh"
      ZPLUG_LOADED_PLUGINS+=("$plugin_url")

      continue
    fi

    # Throw an error if none of sources has been found so user will know
    # that something went wrong with the downloaded plugin
    __zplug_error 'No .plugin.zsh or .zsh file found, most likely, %s is not a valid ZSH plugin' "$plugin_url"
  done
}

function zplug() {
  case "$1" in
    plugin) __zplug_plugin "$2" ;;
    install) __zplug_install ;;
    update) __zplug_update ;;
    load) __zplug_load ;;
    help) __zplug_usage ;;
    *) __zplug_usage ;;
  esac
}
