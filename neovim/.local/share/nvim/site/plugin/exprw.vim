" Vim Plugin:
" File: exprw.vim
" Author: Marco Cantoro
" Description: making netrw less painful
" Last Modified: agosto 27, 2020

" Section: Plugin Guards

if exists('g_loaded_exprw')
  finish
endif
let g:loaded_exprw = 1

" Section: Variable Declaration:

let g:netrw_altv         = 1  " open splits to the right
let g:netrw_banner       = 0  " disable annoying banner
let g:netrw_browse_split = 4  " open in prior window
let g:netrw_liststyle    = 3  " tree view
let g:netrw_winsize      = 15 " window size
let g:netrw_list_hide    = netrw_gitignore#Hide() .
      \ '.git,node_modules,.*\.hi$,.*\.o$,.*\.aux$,.*\.swp$,__pycache__'

let g:exprw_is_open = 0

function! ToggleNetrw() abort " {{{1
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

" Section: Exprw Mappings

nnoremap <silent> <Plug>Exprw :<C-u> call ToggleNetrw()<CR>

if !hasmapto('<Plug>Exprw')
  nmap <unique> <S-F12> <Plug>Exprw
  nmap <unique> <F24>   <Plug>Exprw
endif
