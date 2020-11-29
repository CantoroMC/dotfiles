" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/haskell.vim
" }}}

setlocal nospell

nnoremap <buffer> <LocalLeader>hd 0ywo<Esc>pi =<Esc>k0


let b:undo_ftplugin .= '| setl spell< '
