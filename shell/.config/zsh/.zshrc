# zsh configuration file

#
### fpath and compinit
#
fpath=(
  $ZDOTDIR/functions/Completion
  $ZDOTDIR/functions/Prompts
  $ZDOTDIR/functions/Zle
  $fpath
)

autoload -U compaudit compinit

function handle_completion_insecurities() {
  # List of the absolute paths of all unique insecure directories, split on
  # newline from compaudit()'s output.
  local -aU insecure_dirs
  insecure_dirs=( ${(f@):-"$(compaudit 2>/dev/null)"} )

  # If no such directories exist, get us out of here.
  [[ -z "${insecure_dirs}" ]] && return

  # List ownership and permissions of all insecure directories.
  print "[zsh] Insecure completion-dependent directories detected:"
  ls -ld "${(@)insecure_dirs}"

  cat <<EOD

  [zsh] For safety reason, completions from from these directories we'll not
        be loaded, until you fix their permissions and ownership and restart
        zsh, and compinit is called with the -i flag, to silently ignore
        those insecure directories.

  [zsh] To fix your permissions you can do so by disabling
        the write permission of "group" and "others" and making sure that the
        owner of these directories is either root or your current user.

EOD
}
# If completion insecurities exist, warn the user
handle_completion_insecurities
# Load only from secure directories
compinit -i -C


#
### Zsh library
#
function isMod() {
  builtin test -f $ZDOTDIR/lib/$1.zsh
}

Mods=(
  miniplug
  options
  completion
  functions
  keybindings
  navigation
  aliases
  termtitle
  vcs
  vi_prompt
)
for mod ($Mods); do
  isMod $mod && source $ZDOTDIR/lib/$mod.zsh || \
    printf "[warn] \x1b[33m$mod.zsh not found.\x1b[0m\n" "$@"
done


#
### Zsh plugins
#
miniplug plugin Aloxaf/fzf-tab
miniplug plugin zsh-users/zsh-autosuggestions
miniplug plugin zsh-users/zsh-history-substring-search
miniplug plugin zsh-users/zsh-syntax-highlighting
miniplug load

# Plugins configuration
for miniplug_conf ($MINIPLUG_LOADED_PLUGINS); do
  miniplug_conf="${miniplug_conf##*/}"
  isMod "plug.d/$miniplug_conf" && source $ZDOTDIR/lib/plug.d/$miniplug_conf.zsh || \
    printf "[warn] \x1b[33m$miniplug_conf.zsh not found.\x1b[0m\n" "$@"
done

# Load fzf modules
for plugin (/usr/share/fzf/*.zsh); do
  source $plugin
done

unset miniplug_conf plugin Mods mod
unset -f handle_completion_insecurities isMod


#
### Shell Prompt
#
autoload -Uz promptinit; promptinit; prompt voidy
