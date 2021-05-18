# zsh configuration file


#
## Auxiliary functions
#
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


#
## Variables
#

# Directories to add to fpath
fpath=(
  $ZDOTDIR/functions/Completion
  $ZDOTDIR/functions/Prompts
  $ZDOTDIR/functions/Zle
  $fpath
)

source $ZDOTDIR/miniplug.zsh
miniplug plugin Aloxaf/fzf-tab
miniplug plugin zsh-users/zsh-autosuggestions
miniplug plugin zsh-users/zsh-history-substring-search
miniplug plugin zsh-users/zsh-syntax-highlighting

# Plugins configuration
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'exa --color=always -aT -L=2 --group-directories-first -I=.git $realpath'
zstyle ':fzf-tab:*' switch-group '[' ']'
zstyle ':fzf-tab:complete:cd:*' popup-pad 20 0
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=59'
ZSH_AUTOSUGGEST_USE_ASYNC='true'
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[comment]='fg=59'

# Create a hash table for globally stashing variables without polluting main
# scope with a bunch of identifiers.
typeset -A __BULL

__BULL[ITALIC_ON]=$'\e[3m'
__BULL[ITALIC_OFF]=$'\e[23m'



#
# fpath and compinit
#
autoload -U compaudit compinit

# If completion insecurities exist, warn the user
handle_completion_insecurities
# Load only from secure directories
compinit -i -C

#
# Source the zsh library and plugins
#

# Load all of the config files in ZDOTDIR/lib that end in .zsh
for config_file ($ZDOTDIR/lib/*.zsh); do
  source $config_file
done
unset config_file

miniplug load

# Load fzf completion and key bindings
for plugin (/usr/share/fzf/*.zsh); do
  source $plugin
done
unset plugin plugins


#
# Shell Prompt
#
autoload -Uz promptinit; promptinit; prompt voidy


# zmodload zsh/zpty
