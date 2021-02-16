" plugin/fooldme.vim
" File: fooldme
" Author: Marco Cantoro
" Description: text editing utilities
" Last Modified: february 15, 2021

if exists('g_loaded_fooldme')
  finish
endif
let g:loaded_fooldme = 1

nnoremap <silent> <Plug>FooldmeFMOpen  :<C-U>call fooldme#foldMark(1)<CR>
nnoremap <silent> <Plug>FooldmeFMClose :<C-U>call fooldme#foldMark(0)<CR>

if !hasmapto('<Plug>FooldmeFMOpen', 'n') && maparg('[co','n') ==# ''
  nmap [co <Plug>FooldmeFMOpen
endif
if !hasmapto('<Plug>FooldmeFMClose', 'n') && maparg(']co','n') ==# ''
  nmap ]co <Plug>FooldmeFMClose
endif

nnoremap <silent> <S-Tab> :<C-u>call fooldme#orgCycle()<CR>
