" plugin/kebab.vim
" File: kebab.vim
" Author: Marco Cantoro
" Description: windows, buffers and files management
" Last Modified: Set 20, 20

if exists('g_loaded_kebab')
  finish
endif
let g:loaded_kebab = 1



" Section: cding

command! CdToRoot :execute 'lcd '.kebab#rootDir()
command! CdToDir  :lcd %:h
nnoremap <silent> <Plug>KebabToRoot :<C-U>CdToRoot<CR>
nnoremap <silent> <Plug>KebabToDir  :<C-U>CdToDir<CR>
if !hasmapto('<Plug>KebabToRoot', 'n') && maparg('<Leader>cr', 'n') ==# ''
  nmap <Leader>cr <Plug>KebabToRoot
endif
if !hasmapto('<Plug>KebabToDir', 'n') && maparg('<Leader>cd', 'n') ==# ''
  nmap <Leader>cd <Plug>KebabToDir
endif



" Section: Wrappers

command! -nargs=? -complete=file Evince
      \ call kebab#pdf_reader('evince', <f-args>)
command! -nargs=? -complete=file Zathura
      \ call kebab#pdf_reader('zathura', <f-args>)

command! PandocPdf
      \ :execute '!pandoc --pdf-engine=xelatex -o '
      \ .fnameescape(expand('%:p:r')).'.pdf '
      \ .fnameescape(expand('%:p'))

command! DeletePdf
      \ :call delete(fnameescape(expand('%:p:r')).'.pdf')


command! -nargs=? -complete=file Browse
      \ call kebab#browse(<f-args>)

command! -nargs=1 -complete=file Rm
      \ :execute '!rm '.fnameescape(<q-args>)

command! -nargs=1 -complete=dir Rmdir
      \ :execute '!rmdir '.fnameescape(<q-args>)

command! -nargs=1 -complete=file XdgOpen
      \ :execute '!xdg-open '.fnameescape(<q-args>).' & disown'
