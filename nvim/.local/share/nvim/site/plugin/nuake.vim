" Vim Plugin: {{{1

" File: nuake.vim
" Author: Marco Cantoro
" Description: A Quake-style terminal panel
" Last Modified: agosto 28, 2020

" }}}

" Plugin Guards: {{{1

if exists('g:loaded_nuake') || ( !has('nvim') && !has('patch-8.0.1593') )
  finish
endif
let g:loaded_nuake = 1

" }}}

" User Options: {{{1

let s:options = [
      \ ['position'              , 'bottom'],
      \ ['size'                  , 0.25],
      \ ['per_tab'               , 1],
      \ ['close_if_last_standing', 1],
      \ ['start_insert'          , 1],
      \ ]

for [opt, val] in s:options
  if !exists('g:nuake_' . opt)
    execute 'let g:nuake_' . opt . ' = ' . string(val)
  endif
endfor

" }}}

" Commands And Mappings: {{{1

" Commands: {{{2
command! -nargs=0          Nuake
      \ :call nuake#ToggleWindow()
command! -nargs=0 -count=1 NuakeSendLine
      \ :call nuake#SendLine(<count>)
command! -nargs=0 -count=1 NuakeSendParagraph
      \ :call nuake#SendParagraph(<count>)
command! -nargs=0          NuakeSendBuffer
      \ :call nuake#SendBuffer()
command! -nargs=0          NuakeSendSelection
      \ :call nuake#SendSelection()
" }}}

" Mappings: {{{2

" Sensible Mappings: {{{3
tnoremap        <Esc>  <C-\><C-n>
tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
" }}}

" Nuake Plug Mappings: {{{3
inoremap <silent> <Plug>Nuake <C-\><C-n>:Nuake<CR>
tnoremap <silent> <Plug>Nuake <C-\><C-n>:Nuake<CR>
nnoremap <silent> <Plug>Nuake
      \ :<C-U>call nuake#ToggleWindow()<CR>
nnoremap <silent> <Plug>nuakeSendLine
      \ :<C-U>call nuake#SendLine(v:count1)<CR>
nnoremap <silent> <Plug>nuakeSendParagraph
      \ :<C-U>call nuake#SendParagraph(v:count1)<CR>
nnoremap <silent> <Plug>nuakeSendBuffer
      \ :<C-U>call nuake#SendBuffer()<CR>
xnoremap <silent> <Plug>nuakeSendSelection
      \ :<C-U>call nuake#SendSelection()<CR>
" }}}

" Plugin Re Mappings: {{{3
if !hasmapto('<Plug>Nuake', 'n')
  nmap <unique> <F4> <Plug>Nuake
endif
if !hasmapto('<Plug>Nuake', 'i')
  imap <unique> <F4> <Plug>Nuake
endif
if !hasmapto('<Plug>Nuake', 't')
  tmap <unique> <F4> <Plug>Nuake
endif
if !hasmapto('<Plug>nuakeSendLine','n')
  nmap <unique> <Leader>nl <Plug>nuakeSendLine
endif
if !hasmapto('<Plug>nuakeSendParagraph','n')
  nmap <unique> <Leader>np <Plug>nuakeSendParagraph
endif
if !hasmapto('<Plug>nuakeSendBuffer','n')
  nmap <unique> <Leader>nb <Plug>nuakeSendBuffer
endif
if !hasmapto('<Plug>nuakeSendSelection','x')
  xmap <unique> <Leader>ns <Plug>nuakeSendSelection
endif
" }}}

" }}}

" }}}

" vim:fen:fdm=marker:fdl=0:ts=2:sts=2:et:sw=2:sr
