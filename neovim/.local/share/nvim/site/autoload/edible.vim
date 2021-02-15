" autoload/edible.vim

if exists('g:autoloaded_edible')
  finish
endif
let g:autoloaded_edible = 1


" Section: Text Editing Tricks

function! edible#look_up_or_down(direction) abort
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

let s:ignored_filetypes = get(g:,'edible_stri_ignored_ft',
      \ ['markdown', 'dosini', 'config', 'conf'])
function! edible#strip_trailing_spaces(bang) abort
  if match(s:ignored_filetypes, &filetype) == -1
    let l:currPos = getpos('.')

    if a:bang !=# '!'
      let l:start = ( line('.') - 15 ) >= 1 ? ( line('.') - 15 ) : 1
      let l:end = ( line('.') + 15 ) <= line('$') ? ( line('.') + 15 ) : line('$')
    else
      let l:start = 1
      let l:end = line('$')
    endif

    keepjumps call setline(
          \ l:start,
          \ map(getline(l:start,l:end),
          \   {_,v -> substitute(v, '\s\+$', '', 'e')})
          \ )

    call setpos('.', l:currPos)
  endif
endfunction

function! edible#strip_blank_lines() abort
    let l:curr_pos = getpos('.')

    keeppatterns keepjumps global/^\s*$/delete

    call setpos('.', l:curr_pos)
endfunction

function! edible#titlecase(type, ...) abort
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

function! edible#clear_text(type, ...) abort
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

function! edible#centered_title(count) abort
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

function! edible#lucky_fix_spell(direction, count) abort
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


" Section: Grep with a qflist

function! edible#grep(query) abort
  let query = empty(a:query) ? input('grep!: ') : a:query
  if empty(query)
    redraw
    return
  else
    execute printf('silent grep! %s', escape(query, ' '))
    copen 12
  endif
endfunction

function! edible#grep_operator(type) abort
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
