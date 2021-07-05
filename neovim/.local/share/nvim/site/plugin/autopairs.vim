" plugin/autopairs.vim
" File: autopairs
" Author: Marco Cantoro
" Description: text editing utilities
" Last Modified: agosto 28, 2020

if exists('g_loaded_autopairs')
  finish
endif
let g:loaded_autopairs = 1

let s:autopair_ft = get(g:, 'autopair_filetypes',
      \ {
      \   'parentheses':  ['vim', 'c', 'cpp', 'tex', 'sh', 'ruby', 'zsh', 'haskell', 'lua', 'python'],
      \   'squareBrace':  ['vim', 'c', 'cpp', 'tex', 'sh', 'ruby', 'zsh', 'haskell', 'lua', 'python'],
      \   'curlyBrace':   ['vim', 'ruby', 'zsh', 'ruby', 'sh', 'bash', 'haskell', 'tex', 'lua', 'python', 'css',],
      \   'curlyNewLine': ['vim', 'sh', 'c', 'cpp', 'tex', 'zsh', 'ruby', 'lua', 'css',],
      \   'angleBrace':   ['html',],
      \   'singleQuote':  ['vim', 'c', 'cpp', 'sh','ruby', 'zsh', 'lua', 'python', 'html',],
      \   'doubleQuote':  ['c', 'cpp','ruby', 'tex', 'zsh', 'sh', 'bash', 'haskell', 'lua', 'python', 'html',],
      \ })

augroup autoclose_by_filetype
  autocmd!
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['parentheses'],',')
        \ .' inoremap <buffer> ( ()<C-G>U<Left>'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['squareBrace'],',')
        \ .' inoremap <buffer> [ []<C-G>U<Left>'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['curlyBrace'],',')
        \ .' inoremap <buffer> { {}<C-G>U<Left>'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['curlyNewLine'],',')
        \ .' inoremap <buffer> {<CR> {<CR><CR>}<C-G>U<Up>'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['angleBrace'],',')
        \ .' inoremap <buffer> < <><C-G>U<Left>'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['singleQuote'],',')
        \ ." inoremap <buffer> ' ''<C-G>U<Left>"
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['doubleQuote'],',')
        \ .' inoremap <buffer> " ""<C-G>U<Left>'
augroup END
