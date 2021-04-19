# Directory Stack
setopt autopushd           # Make cd push the old directory onto the directory stack.
setopt autocd              # command issued that can’t be executed as a normal command, if
                           #   is the name of a directory, perform cd instead.
                           # This option is only applicable if the option SHIN_STDIN
                           #   is set, i.e. if commands are being read from standard input.
setopt autoparamslash      # tab completing directory appends a slash
setopt nomatch             # unmatched patterns for filename generation raise an error
setopt pushdignoredups     # Don’t push multiple copies of the same directory onto the directory stack.
setopt pushdminus          # Exchanges the meanings of ‘+’ and ‘-’ when used with a number to specify a
                           #     directory in the stack.
setopt pushdsilent         # Don't print dir stack after pushing/popping

autoload -U colors && colors
if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

# History command configuration
setopt extendedhistory     # record timestamp of command in HISTFILE
setopt histexpiredupsfirst # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt histfindnodups      # don't show dupes when searching
setopt histignoredups      # ignore duplicated commands history list
setopt histignorealldups   # filter non-contiguous duplicates from history
setopt nohistignorespace   # don't ignore commands that start with space
setopt histverify          # show command with history expansion to user before running it
setopt sharehistory        # share command history data

# Misc
setopt auto_resume         # allow simple commands to resume backgrounded jobs
setopt clobber             # allow clobbering with >, no need to use >!
setopt correct             # command auto-correction
setopt correctall          # try to correct the spelling of commands
setopt noignoreeof         # don't prevent accidental C-d from exiting shell
setopt interactivecomments # allow comments even in interactive shells
setopt longlistjobs        # print hjob notifications in the long format.
setopt multios             # perform implicit tees or cats when multiple redirections
                           #   are attempted
setopt noprintexitvalue    # for non-zero exit status
setopt promptsubst         # parameter expansion, command substitution and arithmetic
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
