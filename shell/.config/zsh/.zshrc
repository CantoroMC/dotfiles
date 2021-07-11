# zsh configuration file

#
### fpath and compinit
#
fpath=(
  $ZDOTDIR/functions/Completion
  $ZDOTDIR/functions/Misc
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
  zplug
  options
  completion
  functions
  keybindings
  navigation
  aliases
  termtitle
  vcs
  vi_prompt
  calendar
)
for mod ($Mods); do
  isMod $mod && source $ZDOTDIR/lib/$mod.zsh || \
    printf "[warn] \x1b[33m$mod.zsh not found.\x1b[0m\n" "$@"
done



#
### Shell Prompt
#
autoload -Uz promptinit; promptinit; prompt voidy



#
### Zsh plugins
#
zplug plug Aloxaf/fzf-tab
zplug plug zsh-users/zsh-autosuggestions
zplug plug zsh-users/zsh-history-substring-search
zplug plug zsh-users/zsh-syntax-highlighting
zplug load

# Plugins configuration
for zplug_conf ($zplug_LOADED_PLUGINS); do
  zplug_conf="${zplug_conf##*/}"
  isMod "plug.d/$zplug_conf" && source $ZDOTDIR/lib/plug.d/$zplug_conf.zsh || \
    printf "[warn] \x1b[33m$zplug_conf.zsh not found.\x1b[0m\n" "$@"
done

# Load fzf modules
for plugin (/usr/share/fzf/*.zsh); do
  source $plugin
done

unset zplug_conf plugin Mods mod
unset -f handle_completion_insecurities isMod

zmodload zsh/zpty
