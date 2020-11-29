" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/haskell.vim
" }}}

setlocal nospell

nnoremap <buffer> <LocalLeader>hd 0ywo<Esc>p0


let b:undo_ftplugin .= '| setl spell< '
