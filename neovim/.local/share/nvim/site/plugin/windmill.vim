" plugin/windmill.vim
" File: windmill.vim
" Author: Marco Cantoro
" Description: windows, buffers and files management
" Last Modified: Set 20, 20

if exists('g_loaded_windmill')
  finish
endif
let g:loaded_windmill = 1



" Section: Window Management

" SubSection: Golden Ratio
command! GoldenRatio       :call windmill#golden_ratio()
command! GoldenRatioToggle :call windmill#toggle_golden_ratio()
cnoreabbrev Gold GoldenRatio
cnoreabbrev GRA  GoldenRatioToggle

" SubSection: More window displacement

command! ZoomMode :call windmill#toggle_window_zoom()
nnoremap <silent> <Plug>WindmillToggleWindowZoom :<C-u>call windmill#toggle_window_zoom()<CR>
if !hasmapto('<Plug>WindmillToggleWindowZoom', 'n')
  nmap <C-w>a     <Plug>WindmillToggleWindowZoom
  nmap <C-w><C-a> <Plug>WindmillToggleWindowZoom
endif

command! -nargs=0 -bang FitWinHeight
      \ :call windmill#fit_height(expand('<bang>') != '!')
nnoremap <silent> <Plug>WindmillFitWinHeight :<C-U>FitWinHeight!<CR>
if !hasmapto('<Plug>WindmillFitWinHeight', 'n') &&
      \ maparg('<C-w>gz', 'n') ==# ''
  nmap <C-w>gz <Plug>WindmillFitWinHeight
endif

nnoremap <silent> <Plug>WindmillToggleColorColumn
      \ :<C-U>call windmill#toggle_color_column()<CR>
if !hasmapto('<Plug>WindmillToggleColorColumn', 'n') &&
      \ maparg('ycc', 'n') ==# ''
  nmap ycc <Plug>WindmillToggleColorColumn
endif



" Section: Buffers

" Wipe Buffers
command! BufsWipe :call windmill#wipe_buffers()
nnoremap <silent> <Plug>WindmillBufsWipe :<C-U>BufsWipe<CR>
if !hasmapto('<Plug>WindmillBufsWipe', 'n') && maparg('<Leader>bw', 'n') ==# ''
  nmap <Leader>bw <Plug>WindmillBufsWipe
endif

" Smart Buffer Deletion
command! BufClose :call windmill#close_buffer(1)
nnoremap <silent> <Plug>WindmillBufClose :<C-U>BufClose<CR>
if !hasmapto('<Plug>WindmillBufClose', 'n') && maparg('<Leader>bd', 'n') ==# ''
  nmap <Leader>bd <Plug>WindmillBufClose
endif
