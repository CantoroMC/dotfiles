" autoload/edible.vim

" Auto Loading Guards: {{{1

if exists('g:autoloaded_edible')
  finish
endif
let g:autoloaded_edible = 1

" }}}

" Edible: {{{1
"                                                                            "
"                             /$$ /$$ /$$       /$$                          "
"                            | $$|__/| $$      | $$                          "
"              /$$$$$$   /$$$$$$$ /$$| $$$$$$$ | $$  /$$$$$$                 "
"             /$$__  $$ /$$__  $$| $$| $$__  $$| $$ /$$__  $$                "
"            | $$$$$$$$| $$  | $$| $$| $$  \ $$| $$| $$$$$$$$                "
"            | $$_____/| $$  | $$| $$| $$  | $$| $$| $$_____/                "
"            |  $$$$$$$|  $$$$$$$| $$| $$$$$$$/| $$|  $$$$$$$                "
"             \_______/ \_______/|__/|_______/ |__/ \_______/                "
"                                                                            "
" }}}

" Scratch Buffer: {{{1

function! edible#toggleScratch() abort " {{{2
  let l:scratch_winNr = bufwinnr(g:edible_scratch_name)

  if l:scratch_winNr != -1
    call s:closeScratchWin()
  else
    call s:openScratchWin()
  endif
endfunction
" }}}

function! s:openScratchWin() abort " {{{2
  let l:scratch_bufNr = bufnr(g:edible_scratch_name)
  if l:scratch_bufNr != -1
    let l:scratch_winNr = bufwinnr(g:edible_scratch_name)
    if l:scratch_winNr != -1
      if winnr() != l:scratch_winNr
        execute l:scratch_winNr.'wincmd w'
      else
        return
      endif
    else
      execute 'silent keepalt '.s:ScratchLook().' split '.g:edible_scratch_name
    endif
  else
    execute s:ScratchLook().' new '.g:edible_scratch_name
    call s:InitScratchWin()
  endif
endfunction
" }}}

function! s:closeScratchWin() abort " {{{2
  let l:scratch_winNr = bufwinnr(g:edible_scratch_name)
  if winnr() == l:scratch_winNr
    close
  else
    let l:curr_bufNr = bufnr('%')
    execute l:scratch_winNr.'wincmd w'
    close
    let l:winNr = bufwinnr(l:curr_bufNr)
    if winnr() != l:winNr
      execute l:winNr.'wincmd w'
    endif
  endif
endfunction
" }}}

function! s:InitScratchWin() abort " {{{2
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal buflisted
  setlocal nospell
endfunction
" }}}

function! s:ScratchLook() abort "{{{2
  let l:scratch_winNr = bufwinnr(g:edible_scratch_name)
  if g:edible_scratch_pos ==? 'bottom'
    let l:mode = l:scratch_winNr != -1 ? '' : 'botright '
    let l:size = float2nr(floor(g:edible_scratch_size * (&lines - 2)))
  elseif g:edible_scratch_pos ==? 'top'
    let l:mode = l:scratch_winNr != -1 ? '' : 'topleft '
    let l:size = float2nr(floor(g:edible_scratch_size * (&lines - 2)))
  elseif g:edible_scratch_pos ==? 'right'
    let l:mode = l:scratch_winNr != -1 ? '' : 'botright vertical '
    let l:size = float2nr(floor(g:edible_scratch_size * &columns))
  elseif g:edible_scratch_pos ==? 'left'
    let l:mode = l:scratch_winNr != -1 ? '' : 'topleft vertical '
    let l:size = float2nr(floor(g:edible_scratch_size * &columns))
  endif
  return l:mode.l:size
endfunction
" }}}

" Scratch Auto Commands: {{{2
augroup edible_scratch
  autocmd!
  execute 'autocmd BufNewFile,BufWinEnter '
        \ .g:edible_scratch_name.' call s:InitScratchWin()'
augroup END
" }}}

" }}}

function! edible#look_up_or_down(direction) abort " {{{1
  let l:target_pattern  = '\%' . virtcol('.') . 'v.'

  if a:direction == 1     " Downwards
    let l:target_line_num = search(l:target_pattern . '*\S', 'nW')
  elseif a:direction == 0 " Upwards
    let l:target_line_num = search(l:target_pattern . '*\S', 'bnW')
  else
    return
  endif

  if !l:target_line_num
    return ''
  else
    return matchstr(getline(l:target_line_num), l:target_pattern)
  endif
endfunction
" }}}

function! edible#strip_trailing_spaces() abort " {{{1
  if match(g:edible_ignored_filetypes, &filetype) == -1
    let l:curr_pos = getpos('.')

    retab
    keepjumps call setline(
          \ 1,
          \ map(getline(1,'$'), {_,v -> substitute(v, '\s\+$', '', 'e')})
          \ )

    call setpos('.', l:curr_pos)
  endif
endfunction
" }}}

function! edible#strip_blank_lines() abort " {{{1
    let l:curr_pos = getpos('.')

    keeppatterns keepjumps global/^\s*$/delete

    call setpos('.', l:curr_pos)
endfunction
" }}}

function! edible#titlecase(type, ...) abort " {{{1
  let l:word_pattern    = '\<\(\k\)\(\k*''*\k*\)\>'
  let l:upp_replacement = '\u\1\L\2'

  if a:0
    " Invoked from Visual mode, use '< and '> marks.
    if a:type ==# ''
      silent exe 'normal! `<' . a:type . '`>y'
      let titlecased = substitute(@@, l:word_pattern, l:upp_replacement, 'g')
      call setreg('@', titlecased, 'b')
      silent execute 'normal! ' . a:type . '`>p'
    else
      silent exe 'normal! `<' . a:type . '`>y'
      let @i = substitute(@@, l:word_pattern, l:upp_replacement, 'g')
      silent execute 'normal! ' . a:type . '`>"ip'
    endif
  elseif a:type ==# 'line'
    execute '''[,'']s/'.l:word_pattern.'/'.l:upp_replacement.'/ge'
  else
    silent exe 'normal! `[v`]y'
    let titlecased = substitute(@@, l:word_pattern, l:upp_replacement, 'g')
    silent exe 'normal! v`]c' . titlecased
  endif
endfunction
" }}}

function! edible#clear_text(type, ...) abort " {{{1
  let l:sel_save = &selection
  let &selection = 'inclusive'

  if a:0 " Invoked from Visual mode, use '< and '> marks
    silent exe 'normal! `<'.a:type.'`>r w'
  elseif a:type ==# 'line'
    silent exe "normal! '[V']r w"
  elseif a:type ==# 'block'
    silent exe "normal! `[\<C-V>`]r w"
  elseif a:type ==# 'char'
    silent exe 'normal! `[v`]r w'
  endif

  let &selection = l:sel_save
endfunction
" }}}

function! edible#centered_title(count) abort " {{{1
  let l:curr_line = line('.')
  let l:end_line = l:curr_line + a:count - 1
  call setline(
        \ l:curr_line,
        \ map(
        \   getline(l:curr_line, l:end_line),
        \   {_,line -> substitute(line, '\<.', '\u&', 'g')}
        \   )
        \ )
  execute l:curr_line.','.l:end_line.'center'
  execute '+'.a:count
  silent! call repeat#set("\<Plug>EdibleCenterdTitle", a:count)
endfunction
" }}}

" Foldmarker Insertion: {{{1

function! edible#fold_mark(to_open) abort " {{{2
  let [fo,fc] = s:foldmarkers()
  let lnum = line('.')
  let line = getline(lnum)

  if a:to_open
    call setline(lnum, line.': '.fo)
  else
    call setline(lnum, line.fc)
  endif
  Commentary
  if a:to_open
    silent! call repeat#set("\<Plug>EdibleFoldMarkOpen")
  else
    silent! call repeat#set("\<Plug>EdibleFoldMarkClose")
  endif
endfunction
" }}}

function! s:foldmarkers() abort " {{{2
  return split(&foldmarker, ',')
endfunction
" }}}

" }}}

" Org Mode Folding {{{1

function! edible#tabbed_folding() abort " {{{2
  if foldlevel(line('.')) isnot 0
    if foldclosed(line('.')) isnot -1
      foldopen
    else
      foldclose
    endif
  else
    return
  endif
endfunction
" }}}

function! s:FoldLvlMax() abort " {{{2
  return max(map(range(1,line('$')),
        \ {arg1, arg2 -> foldlevel(arg2) >= foldlevel(arg1) ? foldlevel(arg2) : 0}))
endfunction
" }}}

function! edible#org_folding() abort " {{{2
  if &foldlevel isnot s:FoldLvlMax()
    set foldlevel+=1
    echohl MoreMsg
    echo 'Fold level = '.&foldlevel
    echohl None
  else
    set foldlevel=0
    echohl MoreMsg
    echo 'Folding overview'
    echohl None
  endif
endfunction
" }}}

" }}}

function! edible#lucky_fix_spell(direction, count) abort "{{{1
  let l:curr_pos = getpos('.')

  if a:direction == 1     " Forward
    let l:dir = ']'
  elseif a:direction == 0 " Downward
    let l:dir = '['
  else
    return
  endif

  for _ in range(1, a:count)
    execute 'normal! '.l:dir.'s1z='
  endfor

  call setpos('.', l:curr_pos)

  if a:direction == 1     " Forward
    silent! call repeat#set("\<Plug>EdibleFixNextSpell", a:count)
  elseif a:direction == 0 " Downward
    silent! call repeat#set("\<Plug>EdibleFixPrevSpell", a:count)
  endif
endfunction
" }}}

" Grepping: {{{1

function! edible#grep(query) abort " {{{2
  let query = empty(a:query) ? input('grep!: ') : a:query
  if empty(query)
    redraw
    return
  else
    execute printf('silent grep! %s', escape(query, ' '))
    copen 12
  endif

endfunction
" }}}

function! edible#grep_operator(type) abort " {{{2
  let l:saved_unnamed_register = @@
  if a:type ==# 'v'
    normal! `<v`>y
  elseif a:type ==# 'char'
    normal! `[y`]
  else
    return
  endif

  silent execute 'grep! '.shellescape(@@) .' .'
  botright copen 10

  let @@ = l:saved_unnamed_register
endfunction
" }}}

" }}}
