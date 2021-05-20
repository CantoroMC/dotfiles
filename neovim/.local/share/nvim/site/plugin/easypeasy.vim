" plugin/easypeasy.vim
" File: easypeasy.vim
" Author: Marco Cantoro
" Description: some utilities for Vim, such as session,view,undofile,swap
" management, make dir on editing, find mapping highlight identifier, fast
" modeline insertion
" Last Modified: Feb 14, 21

if exists('g_loaded_easypeasy')
  finish
endif
let g:loaded_easypeasy = 1


" Section: Views Management

command!       RemoveViews    :execute '!rm '.&viewdir.'/*'
command! -bang RemoveCurrView :call easypeasy#delete_view(<q-bang>)
command!       SaveView
      \ if easypeasy#can_be_viewed() |
      \   silent mkview! |
      \ endif
command!       LoadView
      \ if easypeasy#can_be_viewed() |
      \   silent! loadview |
      \ endif


" Sub Section: Sessions Management

let s:session_dir      = get(g:, 'session_dir', stdpath('data').'/sessions/')
let s:def_session_name = get(g:, 'def_session_name', 'last.vim')
let s:last_session = s:session_dir.s:def_session_name

command! -nargs=1 -complete=customlist,easypeasy#comp_session_files SaveSession
      \ :call easypeasy#save_session(<f-args>)
command! -nargs=1 -complete=customlist,easypeasy#comp_session_files LoadSession
      \ :execute 'source '.s:session_dir.<q-args>
command! -nargs=1 -complete=customlist,easypeasy#comp_session_files RemoveSession
      \ :call delete(s:session_dir.<q-args>)
command! -nargs=0 LoadLastSession
      \ :execute 'source '.s:last_session
command! LLS LoadLastSession

nnoremap <silent> <C-c><C-l><C-s> :<C-U>LoadLastSession<CR>

augroup save_session_before_leaving
  autocmd!
  autocmd VimLeave * call easypeasy#save_session()
augroup END


" Section: Undo Files

command! RemoveUndos :execute '!rm '.&undodir.'/*'

" Section: Swap Management

" Section: Swap Management
augroup swap_management
  autocmd!
    autocmd SwapExists *  let v:swapchoice = 'r' | let b:swapname = v:swapname
    autocmd BufWritePost *
          \ if exists("b:swapname") |
          \   call delete(b:swapname) |
          \   unlet b:swapname |
          \ endif
augroup END


" Session: AutoMkdir, findMappedAction, echoHiId

augroup auto_mkdir
  autocmd!
  autocmd BufWritePre * call easypeasy#auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
augroup END

command! FindMappedAction :call easypeasy#retrieve_map()
command! EchoHiId         :call easypeasy#echo_HiId()
command! AddModeLine      :execute
      \ 'normal Go<CR>vim:fdm=marker:ts=2:sts=2:et:sw=2:sr:tw=78<Esc>gcc'

nnoremap <silent> <Leader>eh
      \ :<C-U>call easypeasy#echo_HiId()<CR>

