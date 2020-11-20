" Vim Plugin: {{{1
" File: scriptease.vim
" Author: Marco Cantoro
" Description: Stolen from tpope
" Last Modified: agosto 27, 2020
" }}}

" Plugin Guards: {{{1

if exists("g_loaded_scriptease")
  finish
endif
let g:loaded_scriptease = 1

" }}}

" Commands: {{{1

command! -bar -count=0 Scriptnames
      \ call setqflist(scriptease#scriptnames_qflist()) |
      \ copen |
      \ <count>

" }}}
