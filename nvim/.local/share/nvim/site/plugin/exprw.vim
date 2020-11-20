" Vim Plugin: {{{1
" File: exprw.vim
" Author: Marco Cantoro
" Description: making netrw less painful
" Last Modified: agosto 27, 2020
" }}}

" Plugin Guards: {{{1

if exists('g_loaded_exprw')
  finish
endif
let g:loaded_exprw = 1

" }}}

" Variable Declaration: {{{1

let g:netrw_altv         = 1  " open splits to the right
let g:netrw_banner       = 0  " disable annoying banner
let g:netrw_browse_split = 4  " open in prior window
let g:netrw_liststyle    = 3  " tree view
let g:netrw_winsize      = 15 " window size
" let g:netrw_list_hide
let g:exprw_is_open = 0

" }}}

function! ToggleNetrw() " {{{1
  if g:exprw_is_open
    let i = bufnr("$")
    while (i >= 1)
      if (getbufvar(i, "&filetype") == "netrw")
        silent exe "bwipeout" . i
      endif
      let i-=1
    endwhile
    let g:exprw_is_open=0
  else
    let g:exprw_is_open=1
    silent Lexplore
  endif
endfunction
" }}}

" Netrw Mappings: {{{1

nnoremap <silent> <Plug>Exprw :<C-u> call ToggleNetrw()<CR>

if !hasmapto('<Plug>Exprw')
  nmap <unique> <S-F2> <Plug>Exprw
  nmap <unique> <F14>  <Plug>Exprw
endif

" }}}
