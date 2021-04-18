# The zsh/complist module offers three extensions to completion listings:
#     the ability to high-light matches in such a list ($ZLS_COLORS or :list-colors)
#     the ability to scroll through long lists  ($LISTPROMPT or :listprompt)
#     a different style of menu completion. (select, auto_menu)
zmodload -i zsh/complist

WORDCHARS=''

setopt noflowcontrol    # output flow control via start/stop characters is disabled
setopt nomenucomplete   # do not autoselect the first completion entry
setopt automenu         # show completion menu on after the second consecutive
                        #   request for completion
setopt completeinword   # the cursor stays where it is and completion is done
                        #   from both ends (default)
setopt alwaystoend      # move the cursor to the end of the word when
                        #   completion is performed (default)
setopt listpacked       # make completion lists more densely packed
setopt globdots

zstyle ':completion:*:*:*:*:*' menu select
# Make completion:
# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
zstyle ':completion:*' matcher-list '' \
  '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# Complete . and .. special directories
zstyle ':completion:*' special-dirs '..'
# Colorize completions using default `ls` colors.
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
# Categorize completion suggestions with headings:
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%d'
# zstyle ':completion:*:descriptions' format %F{blue}%B%{$__BULL[ITALIC_ON]%}--- %d ---%{$__BULL[ITALIC_OFF]%}%b%f # don't work with fzf-tab

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
# cd will never select the parent directory
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Completing process IDs with menu selection:
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:*:kill:*:processes' \
  list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:git-checkout:*' sort false

# Fuzzy matching of completions for when you mistype them
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
# number of errors allowed by _approximate increase with length
zstyle -e ':completion:*:approximate:*' \
        max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion:*' use-cache true
# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'
# ... unless we really want to.
zstyle '*' single-ignored show

# Host completion
[[ -r ~/.ssh/config ]] && _ssh_config_hosts=(${${(s: :)${(ps:\t:)${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }}}:#*[*?]*}) || _ssh_config_hosts=()
[[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

hosts=(
  "$(hostname)"
  "$_ssh_config_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts

# automatically load bash completion functions
autoload -U +X bashcompinit && bashcompinit

# Completion for kitty
whence kitty &>/dev/null && kitty + complete setup zsh | source /dev/stdin
