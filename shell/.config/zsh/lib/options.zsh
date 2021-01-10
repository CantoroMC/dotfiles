# Corrections
setopt correct_all            # try to correct the spelling of commands

# Directory Stack
setopt auto_pushd             # Make cd push the old directory onto the directory stack.
setopt auto_cd                # command issued that can’t be executed as a normal command, if 
                              #   is the name of a directory, perform cd instead.
                              # This option is only applicable if the option SHIN_STDIN 
                              #   is set, i.e. if commands are being read from standard input.
setopt pushd_ignore_dups      # Don’t push multiple copies of the same directory onto the directory stack.
setopt pushdminus             # Exchanges the meanings of ‘+’ and ‘-’ when used with a number to specify a 
                              #     directory in the stack.

autoload -U colors && colors
if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

## TODO History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

# Misc
setopt interactivecomments    # allow comments even in interactive shells
setopt long_list_jobs         # print hjob notifications in the long format.
setopt multios                # perform implicit tees or cats when multiple redirections
                              #   are attempted
setopt prompt_subst           # parameter expansion, command substitution and arithmetic
                              #   expansion are performed in prompts

# Widget for support of pasting and url insertion
for d in $fpath; do
  if [[ -e "$d/url-quote-magic" ]]; then
    autoload -Uz bracketed-paste-magic
    zle -N bracketed-paste bracketed-paste-magic
    autoload -Uz url-quote-magic
    zle -N self-insert url-quote-magic
    break
  fi
done
