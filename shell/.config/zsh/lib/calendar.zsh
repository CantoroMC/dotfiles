autoload -Uz calendar{,_edit,_sort,_showdate}

zstyle ':datetime:calendar:' calendar-file ~/.config/zsh/calendar
zstyle ':datetime:calendar:' date-format "%d %a %b %Y %H:%M:%S"
# zstyle ':datetime:calendar:' warn-time 0:15

calendar
