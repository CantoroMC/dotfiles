if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/haskell.vim

command! -buffer HsFormat
      \ :execute '!brittany --write-mode=inplace '.fnameescape(expand('%:p'))

nnoremap <buffer> <silent> <C-c><C-f> :HsFormat<CR>
