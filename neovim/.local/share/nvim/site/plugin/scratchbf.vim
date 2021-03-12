" plugin/scratchbf.vim
if exists('g:loaded_scratchbf')
  finish
endif
let g:loaded_scratchbf = 1

command! Scratch :call scratchbf#toggle()
