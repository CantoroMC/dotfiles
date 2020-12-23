" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/haskell.vim
" }}}

setlocal nospell

command! HCompile :execute '!ghc --make '.fnameescape(expand('%:p'))
command! HFormat  :execute '!brittany --write-mode=inplace '.fnameescape(expand('%:p'))

cabbrev HC HCompile
cabbrev HF HFormat

let b:undo_ftplugin .= '| setl spell< '
