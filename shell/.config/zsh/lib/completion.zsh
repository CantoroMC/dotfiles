# The zsh/complist module offers three extensions to completion listings: 
#     the ability to high-light matches in such a list ($ZLS_COLORS or :list-colors)
#     the ability to scroll through long lists  ($LISTPROMPT or :listprompt)
#     a different style of menu completion. (select, auto_menu)
zmodload -i zsh/complist

WORDCHARS=''

unsetopt flowcontrol      # output flow control via start/stop characters is disabled
unsetopt menu_complete    # do not autoselect the first completion entry
setopt auto_menu          # show completion menu on after the second consecutive
                          #   request for completion
setopt complete_in_word   # the cursor stays where it is and completion is done
                          #   from both ends (default)
setopt always_to_end      # move the cursor to the end of the word when
                          #   completion is performed (default)

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' special-dirs true   # Complete . and .. special directories
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"


# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion:*' use-cache yes
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

# automatically load bash completion functions
autoload -U +X bashcompinit && bashcompinit

# Completion for kitty
whence kitty &>/dev/null && kitty + complete setup zsh | source /dev/stdin
