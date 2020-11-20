" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/haskell.vim
" }}}

setlocal nospell

let b:undo_ftplugin .= '| setl spell< '
