" autoload/nuake.vim

" Auto Loading Guards: {{{1

if exists('g:autoloaded_nuake')
  finish
endif
let g:autoloaded_nuake = 1

" }}}

" Auto Load Functions: {{{1

function! nuake#ToggleWindow() abort "{{{2
  let l:nuake_win_nr = bufwinnr(s:NuakeBufNr())

  if l:nuake_win_nr != -1
    call s:CloseWindow()
  else
    call s:OpenWindow()
  endif
endfunction
" }}}

function! nuake#SendLine(count) abort " {{{2
  call s:OpenWindowAsREPL()
  call chansend(s:nuake_channel, add(getline('.', line('.') + a:count - 1 ), "\n"))
  silent! call repeat#set("\<Plug>nuakeSendLine", a:count)
endfunction
" }}}

function! nuake#SendParagraph(count) abort " {{{2
  call s:OpenWindowAsREPL()
  let l:parag_start = search('^$', 'bnW') == 0 ? 1 : search('^$', 'bnW') + 1
  for i in range(a:count)
    let l:parag_end        = search('^$', 'nW') == 0 ?
          \ line('$') : search('^$', 'nW')
    let l:next_parag_start = search('^.', 'W') == 0 ?
          \ line('$') : search('^.', 'W')
  endfor
  call chansend(s:nuake_channel, add(getline(l:parag_start, l:parag_end), "\n"))
  silent! call repeat#set("\<Plug>nuakeSendParagraph", a:count)
endfunction
" }}}

function! nuake#SendBuffer() abort " {{{2
  call s:OpenWindowAsREPL()
  call chansend(s:nuake_channel, add(getline(1,'$'), "\n"))
  silent! call repeat#set("\<Plug>nuakeSendBuffer")
endfunction
" }}}

function! nuake#SendSelection() abort " {{{2
  let [selL_start, selC_start] = getpos("'<")[1:2]
  let [selL_end, selC_end]     = getpos("'>")[1:2]
  let sel = getline(selL_start, selL_end)
  if len(sel) == 0
      return ''
  endif
  let sel[-1] = sel[-1][: selC_end - (&selection ==# 'inclusive' ? 1 : 2)]
  let sel[0]  = sel[0][selC_start - 1:]

  call s:OpenWindowAsREPL()
  call chansend(s:nuake_channel, add(sel, "\n"))
endfunction
" }}}

" }}}

" Script Functions: {{{1

function! s:NuakeBufNr() abort "{{{2
  if g:nuake_per_tab == 0
    if !exists('s:nuake_buf_nr')
      let s:nuake_buf_nr = -1
    elseif &filetype ==# 'nuake' && s:nuake_buf_nr == -1
      let s:nuake_buf_nr = bufnr('%')
    endif
    return s:nuake_buf_nr
  else
    if !exists('t:nuake_buf_nr')
      let t:nuake_buf_nr = -1
    elseif &filetype ==# 'nuake' && t:nuake_buf_nr == -1
      let t:nuake_buf_nr = bufnr('%')
    endif
    return t:nuake_buf_nr
  endif
endfunction
" }}}

function! s:OpenWindow() abort "{{{2
  let l:nuake_buf_nr = bufnr(s:NuakeBufNr())

  execute 'silent keepalt '.s:NuakeLook().' split'

  if l:nuake_buf_nr != -1
    execute 'buffer '.l:nuake_buf_nr
  else
    let l:vim_options = has('terminal') ? '++curwin ++kill=kill' : ''
    execute 'terminal'.l:vim_options
    call s:InitWindow()
    call s:NuakeBufNr()
  endif

  startinsert!
  let s:nuake_channel = b:terminal_job_id

endfunction
" }}}

function! s:OpenWindowAsREPL() abort "{{{2
  let l:current_buf_nr = bufnr('%')
  let l:win_num        = bufwinnr(l:current_buf_nr)
  let l:nuake_win_nr   = bufwinnr(s:NuakeBufNr())

  if l:nuake_win_nr == -1
    call s:OpenWindow()
    execute l:win_num.'wincmd w'
    stopinsert!
  endif
endfunction
" }}}

function! s:InitWindow() abort "{{{2
  " Global Options:
  setlocal scrolloff=0

  " Local To Buffer Options:
  setlocal filetype=nuake
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nomodified

  " Local To Window Options:
  setlocal nolist
  setlocal nowrap
  setlocal winfixwidth
  setlocal winfixheight
  setlocal nospell
  setlocal nonumber
  setlocal norelativenumber
  setlocal nofoldenable
  setlocal foldcolumn=0
  setlocal signcolumn=no
endfunction
" }}}

function! s:NuakeLook() abort "{{{2
  let l:nuake_win_nr = bufwinnr(s:NuakeBufNr())

  if g:nuake_position ==# 'bottom'
    let l:mode = l:nuake_win_nr != -1 ? '' : 'botright '
    let l:size = float2nr(floor(g:nuake_size * (&lines - 2)))
  elseif g:nuake_position ==# 'top'
    let l:mode = l:nuake_win_nr != -1 ? '' : 'topleft '
    let l:size = float2nr(floor(g:nuake_size * (&lines - 2)))
  elseif g:nuake_position ==# 'right'
    let l:mode = l:nuake_win_nr != -1 ? '' : 'botright vertical '
    let l:size = float2nr(floor(g:nuake_size * &columns))
  elseif g:nuake_position ==# 'left'
    let l:mode = l:nuake_win_nr != -1 ? '' : 'topleft vertical '
    let l:size = float2nr(floor(g:nuake_size * &columns))
  endif

  return l:mode.l:size
endfunction
" }}}

function! s:CloseWindow() abort "{{{2
  let l:nuake_win_nr = bufwinnr(s:NuakeBufNr())

  if winnr() == l:nuake_win_nr
    if winbufnr(2) != -1
      close
    endif
  else
    let l:current_buf_nr = bufnr('%')
    execute l:nuake_win_nr.'wincmd w'
    close

    let l:win_num = bufwinnr(l:current_buf_nr)
    if winnr() != l:win_num
      execute l:win_num . 'wincmd w'
    endif
  endif
endfunction
" }}}

function! s:ResizeWindow() abort "{{{2
  let l:nuake_win_nr = bufwinnr(s:NuakeBufNr())
  execute l:nuake_win_nr.'resize '.s:NuakeLook()
endfunction
" }}}

function! s:LastStandingWindow() abort "{{{2
  if g:nuake_close_if_last_standing == 1
    let l:nuake_win_nr = bufwinnr(s:NuakeBufNr())

    if winnr('$') < 2 && l:nuake_win_nr != -1
      if tabpagenr('$') < 2
        bdelete!
        quit
      else
        close
      endif
    endif
  endif
endfunction

" }}}

function! s:NuakeCloseTab() abort "{{{2
  if s:temp_nuake_buf_nr != -1
    execute 'bdelete! '.s:temp_nuake_buf_nr
    unlet s:temp_nuake_buf_nr
  endif
endfunction
" }}}

" }}}

" Auto Commands: {{{1

augroup nuake_last_standing_window " {{{2
  autocmd!
  autocmd BufEnter * ++nested call s:LastStandingWindow()
augroup END
" }}}

let s:temp_nuake_buf_nr = -1
augroup nuake_tab_close " {{{2
  if g:nuake_per_tab == 1
    autocmd!
    autocmd TabLeave * let s:temp_nuake_buf_nr = bufnr(s:NuakeBufNr())
    autocmd TabClosed * call s:NuakeCloseTab()
  endif
augroup END
" }}}

augroup nuake_term_killed " {{{2
  autocmd!
  autocmd BufDelete *
        \ if bufnr(s:NuakeBufNr()) == -1 |
        \ let s:nuake_buf_nr = -1 |
        \ let t:nuake_buf_nr = -1 |
        \ endif
augroup END
" }}}

augroup nuake_resize_window " {{{2
  autocmd!
  autocmd VimResized *
        \ if bufwinnr(s:NuakeBufNr()) != -1 |
        \ call s:ResizeWindow() | redraw |
        \ endif
augroup END
" }}}

" }}}

" vim:fen:fdm=marker:fdl=0:ts=2:sts=2:et:sw=2:sr
