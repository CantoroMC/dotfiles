# zsh configuration file

function is_plugin() {
  local base_dir=$1
  local name=$2
  builtin test -f $base_dir/plugins/$name/$name.plugin.zsh \
    || builtin test -f $base_dir/plugins/$name/_$name
}

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
[zsh] be loaded, until you fix their permissions and ownership and restart
[zsh] zsh, and compinit is called with the -i flag, to silently ignore
[zsh] those insecure directories.

[zsh] To fix your permissions you can do so by disabling
[zsh] the write permission of "group" and "others" and making sure that the
[zsh] owner of these directories is either root or your current user.

EOD
}


fpath=(
  $ZDOTDIR/functions/Completion
  $ZDOTDIR/functions/Prompts
  $ZDOTDIR/functions/Zle
  $fpath
)

plugins=(
  fzf-tab
  timer
  zsh-autosuggestions
  zsh-history-substring-search
)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=165'
ZSH_AUTOSUGGEST_USE_ASYNC='true'


# fpath and compinit
autoload -U compaudit compinit

for plugin ($plugins); do
  if is_plugin $ZDOTDIR $plugin; then
    fpath=($ZDOTDIR/plugins/$plugin $fpath)
  else
    echo "zsh plugin '$plugin' not found"
  fi
done

# If completion insecurities exist, warn the user
handle_completion_insecurities
# Load only from secure directories
compinit -i -C

# Source the zsh library and plugins

# # Load all of the config files in ZDOTDIR/lib that end in .zsh
for config_file ($ZDOTDIR/lib/*.zsh); do
  source $config_file
done
unset config_file

# Load all of the desired plugins
for plugin ($plugins); do
  if [ -f $ZDOTDIR/plugins/$plugin/$plugin.plugin.zsh ]; then
    source $ZDOTDIR/plugins/$plugin/$plugin.plugin.zsh
  fi
done

# Load fzf completion and key bindings from the vim plugin
for plugin ($XDG_DATA_HOME/nvim/plugged/fzf/shell/*.zsh); do
  source $plugin
done
unset plugin plugins

# Load the Shell Prompt Theme
autoload -Uz promptinit; promptinit; prompt bull blue white yellow
