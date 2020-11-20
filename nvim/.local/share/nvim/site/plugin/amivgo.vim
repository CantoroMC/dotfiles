" Vim Plugin: {{{1

" File: amivgo
" Author: Marco Cantoro
" Description: some utilities for Vim
" Last Modified: Set 21, 20

" }}}

" Plugin Guards: {{{1

if exists('g_loaded_amivgo')
  finish
endif
let g:loaded_amivgo = 1

" }}}

" AmiVgo: {{{1
"                                                                            "
"                                 /$$                                        "
"                                |__/                                        "
"          /$$$$$$  /$$$$$$/$$$$  /$$ /$$    /$$ /$$$$$$   /$$$$$$           "
"         |____  $$| $$_  $$_  $$| $$|  $$  /$$//$$__  $$ /$$__  $$          "
"          /$$$$$$$| $$ \ $$ \ $$| $$ \  $$/$$/| $$  \ $$| $$  \ $$          "
"         /$$__  $$| $$ | $$ | $$| $$  \  $$$/ | $$  | $$| $$  | $$          "
"        |  $$$$$$$| $$ | $$ | $$| $$   \  $/  |  $$$$$$$|  $$$$$$/          "
"         \_______/|__/ |__/ |__/|__/    \_/    \____  $$ \______/           "
"                                               /$$  \ $$                    "
"                                              |  $$$$$$/                    "
"                                               \______/                     "
"                                                                            "
" }}}

" Section: Adds

" Number Conversions: {{{1

command! -nargs=1 EvalAsBin echomsg amivgo#eval_base_number(2,  <f-args>)
command! -nargs=1 EvalAsOct echomsg amivgo#eval_base_number(8,  <f-args>)
command! -nargs=1 EvalAsDec echomsg amivgo#eval_base_number(10, <f-args>)
command! -nargs=1 EvalAsHex echomsg amivgo#eval_base_number(16, <f-args>)
if has('nvim')
  command! -nargs=0 BaseNumbers
        \ :call amivgo#float_base_numbers(expand('<cword>'))
endif

inoremap <silent><expr> <C-G>b
      \ amivgo#eval_base_number(2, input('= '))
inoremap <silent><expr> <C-G>o
      \ amivgo#eval_base_number(8, input('= '))
inoremap <silent><expr> <C-G>d
      \ amivgo#eval_base_number(10, input('= '))
inoremap <silent><expr> <C-G>h
      \ amivgo#eval_base_number(16, input('= '))

imap <silent> <C-G><C-B> <C-G>b
imap <silent> <C-G><C-O> <C-G>o
imap <silent> <C-G><C-D> <C-G>d
imap <silent> <C-G><C-H> <C-G>h

" }}}

" Get Character Encoding: {{{1
command! GetCharEnc :call amivgo#get_char_encoding()
" }}}

" Generate UUID: {{{1

inoremap <silent> <Plug>AmivgoUUID <C-r>=amivgo#uuid()<CR>

if !hasmapto('<Plug>AmivgoUUID', 'i') && maparg('<F2>', 'i') ==# ''
  imap <F2> <Plug>AmivgoUUID
endif

" }}}

" Section: Vim management

augroup swap_management " {{{1
  autocmd!
    autocmd SwapExists *  let v:swapchoice = 'r' | let b:swapname = v:swapname
    autocmd BufWritePost *
          \ if exists("b:swapname") |
          \   call delete(b:swapname) |
          \   unlet b:swapname |
          \ endif
augroup END
" }}}

augroup auto_mkdir " {{{1
  autocmd!
  autocmd BufWritePre * call amivgo#auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
augroup END
" }}}

" Current Item Syntax Info: {{{1

command! EchoHiId :call amivgo#echo_hi_id()

nnoremap <silent> <Plug>AmivgoEchoHiId :<C-U>call amivgo#echo_hi_id()<CR>

if !hasmapto('<Plug>AmivgoEchoHiId', 'n') && maparg('<Leader>eh', 'n') ==# ''
  nmap <Leader>eh <Plug>AmivgoEchoHiId
endif

" }}}

" Retrieve Map: {{{1
command! RetrieveMap :call amivgo#retrieve_map()
" }}}

" Clear Registers: {{{1
command! ClearRegisters :call amivgo#clear_registers()
" }}}

" Insert Mode Line: {{{1

command! -nargs=0 AddModeLine :execute
      \ 'normal Go<CR>vim:fdm=marker:ts=2:sts=2:et:sw=2:sr:tw=78<Esc>gcc'
nnoremap <silent> <Plug>AmivgoModeLine :<C-U>call AddModeLine

if !hasmapto('<Plug>AmivgoModeLine', 'n') && maparg('<Leader>vm', 'n') ==# ''
  nmap <Leader>vm <Plug>AmivgoModeLine
endif

" }}}

" Frequently Used Configuration Files: FreqCfgs {{{1
if !exists('g:amivgo_freq_cfgs')
  let g:amivgo_freq_cfgs = {}
endif

command! -bang -nargs=1 -complete=custom,amivgo#comp_freq_cfgs FreqCfgs
      \ if <bang>1 |
      \   let view = 'split' |
      \ else |
      \   let view = 'tab split' |
      \ endif |
      \ execute view.' $HOME/'.g:amivgo_freq_cfgs[<q-args>]
" }}}

" Remove Undo Files: {{{1
command! RemoveUndos :execute '!rm '.&undodir.'/*'
" }}}

" Source Vim Scripts: file, lines and selection {{{1

" Files: {{{2

command! SourceVimRc    :source $MYVIMRC
command! SourceCurrFile :call amivgo#source_vim_script('%')

nnoremap <silent> <Plug>AmivgoSourceVimRc    :<C-U>SourceVimRc<CR>
nnoremap <silent> <Plug>AmivgoSourceCurrFile :<C-U>SourceCurrFile<CR>

if !hasmapto('<Plug>AmivgoSourceVimRc', 'n') &&
      \ maparg('<Leader>sv', 'n') ==# ''
  nmap <Leader>sv <Plug>AmivgoSourceVimRc
endif
if !hasmapto('<Plug>AmivgoSourceCurrFile', 'n') &&
      \ maparg('<Leader>sf', 'n') ==# ''
  nmap <Leader>sf <Plug>AmivgoSourceCurrFile
endif

" }}}

" Lines: {{{2

command! -range VimExecute
      \ if getbufvar(expand('%'), '&filetype') ==# 'vim' |
      \   for i in range(<line1>,<line2>) |
      \     execute getline(i) |
      \   endfor |
      \ endif

nnoremap <silent> <Plug>AmivgoEvalLines
      \ :call amivgo#eval_lines()<CR>
xnoremap <silent> <Plug>AmivgoEvalLines
      \ :call amivgo#eval_lines()<CR>

if !hasmapto('<Plug>AmivgoEvalLines', 'n') &&
      \ maparg('<Leader>sl', 'n') ==# ''
  nmap <Leader>sl <Plug>AmivgoEvalLines
endif
if !hasmapto('<Plug>AmivgoEvalLines', 'x') &&
      \ maparg('<Leader>sl', 'x') ==# ''
  xmap <Leader>sl <Plug>AmivgoEvalLines
endif
" }}}

" }}}

" Sub Section: Sessions Management {{{1

" Global Variables: {{{2

if !exists('g:session_dir')
  let g:session_dir = $XDG_CACHE_HOME.'/nvim/sessions/'
endif

if !exists('g:last_session_name')
  let g:last_session_name = 'last_session.vim'
endif
let g:last_session = g:session_dir.g:last_session_name

" }}}

command! -nargs=? -complete=customlist,amivgo#comp_session_files SaveSession
      \ :call amivgo#save_session(<f-args>)
command! -nargs=1 -complete=customlist,amivgo#comp_session_files LoadSession
      \ :execute 'source'.g:session_dir.<q-args>
command! -nargs=1 -complete=customlist,amivgo#comp_session_files RemoveSession
      \ :call delete(g:session_dir.<q-args>)
command! -nargs=0 LoadLastSession
      \ :execute 'source '.g:last_session

augroup save_session_before_leaving " {{{2
  autocmd!
  autocmd VimLeave * call amivgo#save_session()
augroup END
" }}}

" }}}

" Sub Section: View Management {{{1
command!       RemoveViews    :execute '!rm '.&viewdir.'/*'
command! -bang RemoveCurrView :call amivgo#delete_view(<q-bang>)
command!       SaveView
      \ if amivgo#is_view_available() |
      \   silent mkview! |
      \ endif
command!       LoadView
      \ if amivgo#is_view_available() |
      \   silent! loadview |
      \ endif
" }}}
