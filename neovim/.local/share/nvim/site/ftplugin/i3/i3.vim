" ftplugin/i3/i3.vim
" Language:             i3 configuration files
" Previous Maintainer:  Marco Cantoro
" Latest Revision:      Ott 22, 20


" Section: Filetype plugin guards

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1


" Section: Filetype options

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


" Section: Options Restoring

let b:undo_ftplugin = "setl com< cms< fo< spell< fdm< ts< sts< et< sw< tw< "
