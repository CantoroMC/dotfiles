" Vim Plugin: {{{1

" File: windmill
" Author: Marco Cantoro
" Description: windows, buffers and files management
" Last Modified: Set 20, 20

" }}}

" Plugin Guards: {{{1

if exists('g_loaded_windmill')
  finish
endif
let g:loaded_windmill = 1

" }}}

" Windmill: {{{1
"                                                                             "
"                      /$$                 /$$               /$$ /$$ /$$      "
"                     |__/                | $$              |__/| $$| $$      "
"        /$$  /$$  /$$ /$$ /$$$$$$$   /$$$$$$$ /$$$$$$/$$$$  /$$| $$| $$      "
"       | $$ | $$ | $$| $$| $$__  $$ /$$__  $$| $$_  $$_  $$| $$| $$| $$      "
"       | $$ | $$ | $$| $$| $$  \ $$| $$  | $$| $$ \ $$ \ $$| $$| $$| $$      "
"       | $$ | $$ | $$| $$| $$  | $$| $$  | $$| $$ | $$ | $$| $$| $$| $$      "
"       |  $$$$$/$$$$/| $$| $$  | $$|  $$$$$$$| $$ | $$ | $$| $$| $$| $$      "
"        \_____/\___/ |__/|__/  |__/ \_______/|__/ |__/ |__/|__/|__/|__/      "
"                                                                             "
" }}}

" Section: Windows

" Resize To Golden Ratio: {{{1

command! GoldenRatio       :call windmill#golden_ratio()
command! GoldenRatioToggle :call windmill#toggle_golden_ratio()
cnoreabbrev GR GoldenRatio
cnoreabbrev GRT GoldenRatioToggle

" }}}

" Window Zoom: {{{1

command! ToggleWindowZoom :call windmill#toggle_window_zoom()

nnoremap <silent> <Plug>WindmillToggleWindowZoom
      \ :<C-u>call windmill#toggle_window_zoom()<CR>

if !hasmapto('<Plug>WindmillToggleWindowZoom', 'n')
  nmap <C-w>a     <Plug>WindmillToggleWindowZoom
  nmap <C-w><C-a> <Plug>WindmillToggleWindowZoom
endif
" }}}

" Fit Window Height: {{{1

command! -nargs=0 -bang FitWinHeight
      \ :call windmill#fit_height(expand('<bang>') != '!')

nnoremap <silent> <Plug>WindmillFitWinHeight :<C-U>FitWinHeight!<CR>

if !hasmapto('<Plug>WindmillFitWinHeight', 'n') &&
      \ maparg('<Leader>fw', 'n') ==# ''
  nmap <Leader>fw <Plug>WindmillFitWinHeight
endif

" }}}

" Toggle Color Column: {{{1

nnoremap <silent> <Plug>WindmillToggleColorColumn
      \ :<C-U>call windmill#toggle_color_column()<CR>

if !hasmapto('<Plug>WindmillToggleColorColumn', 'n') &&
      \ maparg('ycc', 'n') ==# ''
  nmap ycc <Plug>WindmillToggleColorColumn
endif

" }}}

" Section: Files

" Switching Directory: {{{1

command! CdToRoot :execute 'lcd '.windmill#rootDir()
command! CdToDir  :lcd %:h

nnoremap <silent> <Plug>WindmillToRoot :<C-U>CdToRoot<CR>
nnoremap <silent> <Plug>WindmillToDir  :<C-U>CdToDir<CR>

if !hasmapto('<Plug>WindmillToRoot', 'n') && maparg('<Leader>cr', 'n') ==# ''
  nmap <Leader>cr <Plug>WindmillToRoot
endif
if !hasmapto('<Plug>WindmillToDir', 'n') && maparg('<Leader>cd', 'n') ==# ''
  nmap <Leader>cd <Plug>WindmillToDir
endif

" }}}

" Copy File Name To Clipboard: {{{1
command! -nargs=? -complete=custom,windmill#comp_path_mod ClipFile
      \ :call windmill#clip_filename(<f-args>)
" }}}

" Handle Related Pdf: {{{1

command! -nargs=? -complete=file Evince
      \ call windmill#pdf_reader('evince', <f-args>)
command! -nargs=? -complete=file Zathura
      \ call windmill#pdf_reader('zathura', <f-args>)
command! RemoveAssociatedPdf
      \ :call delete(fnameescape(expand('%:p:r')).'.pdf')

" }}}

" Browse {{{1
command! Browse :execute '!vivaldi-stable '
      \ .fnameescape(expand('%:p')).' & disown'
command! -nargs=1 -complete=file XdgOpen
      \ :execute '!xdg-open '.fnameescape(<q-args>)
" }}}

" Section: Buffers

" Wipe Buffers And Smart Buffer Deletion: {{{1

command! BufsWipe :call windmill#wipe_buffers()
command! BufClose :call windmill#close_buffer(1)

nnoremap <silent> <Plug>WindmillBufsWipe :<C-U>BufsWipe<CR>
nnoremap <silent> <Plug>WindmillBufClose :<C-U>BufClose<CR>

if !hasmapto('<Plug>WindmillBufsWipe', 'n') && maparg('<Leader>bw', 'n') ==# ''
  nmap <Leader>bw <Plug>WindmillBufsWipe
endif
if !hasmapto('<Plug>WindmillBufClose', 'n') && maparg('<Leader>bd', 'n') ==# ''
  nmap <Leader>bd <Plug>WindmillBufClose
endif
" }}}
