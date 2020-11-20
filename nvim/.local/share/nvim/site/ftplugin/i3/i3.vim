" Vim Filetype Plugin File: {{{1
" Language:             i3 configuration files
" Previous Maintainer:  Marco Cantoro
" Latest Revision:      Ott 22, 20
" }}}

" File Type Plugin Guards: {{{1
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1
" }}}

" Filetype Options: {{{1
setlocal comments=:#
setlocal commentstring=#\ %s
setlocal formatoptions-=t
setlocal formatoptions+=croql
setlocal nospell
setlocal foldmethod=marker
setlocal tabstop=2
setlocal softtabstop=2
setlocal expandtab
setlocal shiftwidth=2
setlocal textwidth=0
" }}}

let b:undo_ftplugin = "setl com< cms< fo< spell< fdm< ts< sts< et< sw< tw< "
