" plugin/evalnu.vim
" File: evalnu.vim
" Author: Marco Cantoro
" Description:
" Last Modified: Feb 14, 21

if exists('g_loaded_evalnu')
  finish
endif
let g:loaded_evalnu = 1

command! -nargs=1 EvalAsBin echomsg evalnu#eval_base_number(2,  <f-args>)
command! -nargs=1 EvalAsOct echomsg evalnu#eval_base_number(8,  <f-args>)
command! -nargs=1 EvalAsDec echomsg evalnu#eval_base_number(10, <f-args>)
command! -nargs=1 EvalAsHex echomsg evalnu#eval_base_number(16, <f-args>)

command! -nargs=0 BaseNumbers
      \ :call evalnu#float_base_numbers(expand('<cword>'))
nnoremap <silent> <Leader>ga :<C-U>BaseNumbers<CR>
command! GetCharEnc :call evalnu#get_char_encoding()

inoremap <silent> <F2> <C-r>=evalnu#uuid()<CR>
inoremap <silent><expr> <C-G>b evalnu#eval_base_number(2, input('= '))
inoremap <silent><expr> <C-G>o evalnu#eval_base_number(8, input('= '))
inoremap <silent><expr> <C-G>d evalnu#eval_base_number(10, input('= '))
inoremap <silent><expr> <C-G>h evalnu#eval_base_number(16, input('= '))
imap <silent> <C-G><C-B> <C-G>b
imap <silent> <C-G><C-O> <C-G>o
imap <silent> <C-G><C-D> <C-G>d
imap <silent> <C-G><C-H> <C-G>h
