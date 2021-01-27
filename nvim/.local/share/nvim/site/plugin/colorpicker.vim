" plugin/colorpicker.vim

" Vim Plugin: {{{1
" File: colorpicker.vim
" Author: Marco Cantoro <marco.cantoro92@outlook.it>
" Description:
" Last Modified: 2020-10-14
" }}}

" Plugin Guards: {{{1

if exists('g:loaded_colorpicker')
  finish
endif
let g:loaded_colorpicker = 1

" }}}

let g:colorpicker_themes = get(
      \ g:, 'colorpicker_themes',
      \   { 'dark': {
      \       'ayu'    : [ 'ayu_dark', 1 ],
      \       'gruvbox': [ 'base16_gruvbox_dark_hard', 1 ],
      \     },
      \     'light': {
      \       'ayu'    : [ 'ayu_light', 1 ],
      \       'gruvbox': [ 'gruvbox', 1 ],
      \     },
      \   }
      \ )

command! -nargs=* -complete=custom,s:comp_ColorScheme ColorScheme
      \ :call colorpicker#pickIt(<f-args>)
command! -nargs=0 ColorRandom
      \ :call colorpicker#pickIt()
command! -nargs=0 Dark
      \ :call colorpicker#pickIt('dark')
command! -nargs=0 Light
      \ :call colorpicker#pickIt('light')

function! s:comp_ColorScheme(A, L, P) "{{{1
  if a:L ==# 'ColorScheme '.a:A
    return join(['dark', 'light'], "\n")."\n"
  else
    return join(sort(keys(g:colorpicker_themes[split(a:L)[1]]), 'i'), "\n")."\n"
  endif
endfunction
" }}}
