" Vim Plugin: {{{

" File:             unimpaired.vim
" Author:           Tim Pope <http://tpo.pe/>
" Maintainer:       Marco Cantoro
" Description:      Pairs of handy bracket mappings
" Last Modified:    July 20, 2020

" }}}

" Plugin Guards: {{{1

if exists('g:loaded_unimpaired') || &cp || v:version < 700
  finish
endif
let g:loaded_unimpaired = 1

" }}}

" Mappings Functions: {{{1

let s:maps = []
function! s:map(...) abort
  call add(s:maps, copy(a:000))
endfunction

function! s:maps() abort
  for [mode, head, rhs; rest] in s:maps
    let flags = get(rest, 0, '') . (rhs =~# '^<Plug>' ? '' : '<script>')
    let tail = ''
    let keys = get(g:, mode.'remap', {})
    if type(keys) != type({})
      continue
    endif
    while !empty(head) && len(keys)
      if has_key(keys, head)
        let head = keys[head]
        if empty(head)
          let head = '<skip>'
        endif
        break
      endif
      let tail = matchstr(head, '<[^<>]*>$\|.$') . tail
      let head = substitute(head, '<[^<>]*>$\|.$', '', '')
    endwhile
    if head !=# '<skip>' && empty(maparg(head.tail, mode))
      exe mode.'map' flags head.tail rhs
    endif
  endfor
endfunction

" }}}

" Next And Previous: {{{1

" ArgList BufList LocationList QuickFixList TagList: {{{2
function! s:MapNextFamily(map,cmd) abort
  let map = '<Plug>unimpaired'.toupper(a:map)
  let cmd = '".(v:count ? v:count : "")."'.a:cmd
  let end = '"<CR>'.(a:cmd ==# 'l' || a:cmd ==# 'c' ? 'zv' : '')

  execute 'nnoremap <silent> '.map.'Previous :<C-U>exe "'.cmd.'previous'.end
  execute 'nnoremap <silent> '.map.'Next     :<C-U>exe "'.cmd.'next'.end
  execute 'nnoremap <silent> '.map.'First    :<C-U>exe "'.cmd.'first'.end
  execute 'nnoremap <silent> '.map.'Last     :<C-U>exe "'.cmd.'last'.end

  call s:map('n', '['.        a:map , map.'Previous')
  call s:map('n', ']'.        a:map , map.'Next')
  call s:map('n', '['.toupper(a:map), map.'First')
  call s:map('n', ']'.toupper(a:map), map.'Last')

  if exists(':'.a:cmd.'nfile')
    execute 'nnoremap <silent> '.map.'PFile :<C-U>exe "'.cmd.'pfile'.end
    execute 'nnoremap <silent> '.map.'NFile :<C-U>exe "'.cmd.'nfile'.end
    call s:map('n', '[<C-'.toupper(a:map).'>', map.'PFile')
    call s:map('n', ']<C-'.toupper(a:map).'>', map.'NFile')
  elseif exists(':p'.a:cmd.'next')
    execute 'nnoremap <silent> '.map.'PPrevious :<C-U>exe "p'.cmd.'previous'.end
    execute 'nnoremap <silent> '.map.'PNext :<C-U>exe "p'.cmd.'next'.end
    call s:map('n', '[<C-'.toupper(a:map).'>', map.'PPrevious')
    call s:map('n', ']<C-'.toupper(a:map).'>', map.'PNext')
  endif
endfunction

call s:MapNextFamily('a','')
call s:MapNextFamily('b','b')
call s:MapNextFamily('l','l')
call s:MapNextFamily('q','c')
call s:MapNextFamily('t','t')
" }}}

" File Of The Current Directory: {{{2
function! s:entries(path) abort
  let path = substitute(a:path,'[\\/]$','','')
  let files = split(glob(path."/.*"),"\n")
  let files += split(glob(path."/*"),"\n")
  call map(files,'substitute(v:val,"[\\/]$","","")')
  call filter(files,'v:val !~# "[\\\\/]\\.\\.\\=$"')

  let filter_suffixes = substitute(escape(&suffixes, '~.*$^'), ',', '$\\|', 'g') .'$'
  call filter(files, 'v:val !~# filter_suffixes')

  return files
endfunction

function! s:FileByOffset(num) abort
  let file = expand('%:p')
  if empty(file)
    let file = getcwd() . '/'
  endif
  let num = a:num
  while num
    let files = s:entries(fnamemodify(file,':h'))
    if a:num < 0
      call reverse(sort(filter(files,'v:val <# file')))
    else
      call sort(filter(files,'v:val ># file'))
    endif
    let temp = get(files,0,'')
    if empty(temp)
      let file = fnamemodify(file,':h')
    else
      let file = temp
      let found = 1
      while isdirectory(file)
        let files = s:entries(file)
        if empty(files)
          let found = 0
          break
        endif
        let file = files[num > 0 ? 0 : -1]
      endwhile
      let num += (num > 0 ? -1 : 1) * found
    endif
  endwhile
  return file
endfunction

function! s:fnameescape(file) abort
  if exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file," \t\n*?[{`$\\%#'\"|!<")
  endif
endfunction

nnoremap <silent> <Plug>unimpairedDirectoryNext     :<C-U>edit <C-R>=<SID>fnameescape(fnamemodify(<SID>FileByOffset(v:count1), ':.'))<CR><CR>
nnoremap <silent> <Plug>unimpairedDirectoryPrevious :<C-U>edit <C-R>=<SID>fnameescape(fnamemodify(<SID>FileByOffset(-v:count1), ':.'))<CR><CR>

call s:map('n', ']f', '<Plug>unimpairedDirectoryNext')
call s:map('n', '[f', '<Plug>unimpairedDirectoryPrevious')
" }}}

" }}}

" Differences Or Conflicts: {{{1

function! s:Context(reverse) abort
  call search('^\(@@ .* @@\|[<=>|]\{7}[<=>|]\@!\)', a:reverse ? 'bW' : 'W')
endfunction

function! s:ContextMotion(reverse) abort
  if a:reverse
    -
  endif
  call search('^@@ .* @@\|^diff \|^[<=>|]\{7}[<=>|]\@!', 'bWc')
  if getline('.') =~# '^diff '
    let end = search('^diff ', 'Wn') - 1
    if end < 0
      let end = line('$')
    endif
  elseif getline('.') =~# '^@@ '
    let end = search('^@@ .* @@\|^diff ', 'Wn') - 1
    if end < 0
      let end = line('$')
    endif
  elseif getline('.') =~# '^=\{7\}'
    +
    let end = search('^>\{7}>\@!', 'Wnc')
  elseif getline('.') =~# '^[<=>|]\{7\}'
    let end = search('^[<=>|]\{7}[<=>|]\@!', 'Wn') - 1
  else
    return
  endif
  if end > line('.')
    execute 'normal! V'.(end - line('.')).'j'
  elseif end == line('.')
    normal! V
  endif
endfunction

nnoremap <silent> <Plug>unimpairedContextPrevious :<C-U>call <SID>Context(1)<CR>
nnoremap <silent> <Plug>unimpairedContextNext     :<C-U>call <SID>Context(0)<CR>
xnoremap <silent> <Plug>unimpairedContextPrevious :<C-U>exe 'normal! gv'<Bar>call <SID>Context(1)<CR>
xnoremap <silent> <Plug>unimpairedContextNext     :<C-U>exe 'normal! gv'<Bar>call <SID>Context(0)<CR>
onoremap <silent> <Plug>unimpairedContextPrevious :<C-U>call <SID>ContextMotion(1)<CR>
onoremap <silent> <Plug>unimpairedContextNext     :<C-U>call <SID>ContextMotion(0)<CR>

call s:map('n', '[n', '<Plug>unimpairedContextPrevious')
call s:map('n', ']n', '<Plug>unimpairedContextNext')
call s:map('x', '[n', '<Plug>unimpairedContextPrevious')
call s:map('x', ']n', '<Plug>unimpairedContextNext')
call s:map('o', '[n', '<Plug>unimpairedContextPrevious')
call s:map('o', ']n', '<Plug>unimpairedContextNext')

" }}}

" Line Operations: {{{1

" Blank Lines: {{{2
function! s:BlankUp(count) abort
  put!=repeat(nr2char(10), a:count)
  ']+1
  silent! call repeat#set("\<Plug>unimpairedBlankUp", a:count)
endfunction

function! s:BlankDown(count) abort
  put =repeat(nr2char(10), a:count)
  '[-1
  silent! call repeat#set("\<Plug>unimpairedBlankDown", a:count)
endfunction

nnoremap <silent> <Plug>unimpairedBlankUp   :<C-U>call <SID>BlankUp(v:count1)<CR>
nnoremap <silent> <Plug>unimpairedBlankDown :<C-U>call <SID>BlankDown(v:count1)<CR>

call s:map('n', '[<Space>', '<Plug>unimpairedBlankUp')
call s:map('n', ']<Space>', '<Plug>unimpairedBlankDown')
" }}}

" Moving: {{{2
function! s:ExecMove(cmd) abort
  let old_fdm = &foldmethod
  if old_fdm !=# 'manual'
    let &foldmethod = 'manual'
  endif
  normal! m`
  silent! exe a:cmd
  norm! ``
  if old_fdm !=# 'manual'
    let &foldmethod = old_fdm
  endif
endfunction

function! s:Move(cmd, count, map) abort
  call s:ExecMove('move'.a:cmd.a:count)
  silent! call repeat#set("\<Plug>unimpairedMove".a:map, a:count)
endfunction

function! s:MoveSelectionUp(count) abort
  call s:ExecMove("'<,'>move'<--".a:count)
  silent! call repeat#set("\<Plug>unimpairedMoveSelectionUp", a:count)
endfunction

function! s:MoveSelectionDown(count) abort
  call s:ExecMove("'<,'>move'>+".a:count)
  silent! call repeat#set("\<Plug>unimpairedMoveSelectionDown", a:count)
endfunction

nnoremap <silent> <Plug>unimpairedMoveUp            :<C-U>call <SID>Move('--',v:count1,'Up')<CR>
nnoremap <silent> <Plug>unimpairedMoveDown          :<C-U>call <SID>Move('+',v:count1,'Down')<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionUp   :<C-U>call <SID>MoveSelectionUp(v:count1)<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionDown :<C-U>call <SID>MoveSelectionDown(v:count1)<CR>

call s:map('n', '[e', '<Plug>unimpairedMoveUp')
call s:map('n', ']e', '<Plug>unimpairedMoveDown')
call s:map('x', '[e', '<Plug>unimpairedMoveSelectionUp')
call s:map('x', ']e', '<Plug>unimpairedMoveSelectionDown')
" }}}

" }}}

" Option Toggling: {{{1

" Auxiliary Functions: {{{2

function! s:statusbump() abort
  let &l:readonly = &l:readonly
  return ''
endfunction

function! s:toggle(op) abort
  call s:statusbump()
  return eval('&'.a:op) ? 'no'.a:op : a:op
endfunction

function! s:option_map(letter, option, mode) abort
  call s:map('n', '[o'.a:letter, ':'.a:mode.' '
        \ .a:option.'<C-R>=<SID>statusbump()<CR><CR>')
  call s:map('n', ']o'.a:letter, ':'.a:mode.' no'
        \ .a:option.'<C-R>=<SID>statusbump()<CR><CR>')
  call s:map('n', 'yo'.a:letter, ':'.a:mode
        \ .' <C-R>=<SID>toggle("'.a:option.'")<CR><CR>')
endfunction

function! s:cursor_options() abort " {{{
  return &cursorline && &cursorcolumn ?
        \ 'nocursorline nocursorcolumn' : 'cursorline cursorcolumn'
endfunction
" }}}

function! s:word_processing_options() abort " {{{
  if !exists('b:word_processing') || b:word_processing == 1
    setlocal nowrap nolinebreak list textwidth=80
    let b:word_processing = 0
  else
    setlocal wrap linebreak nolist textwidth=0
    let b:word_processing = 1
  endif
endfunction
" }}}

function! s:left_columns_options() abort " {{{
  if !exists('b:left_columns') || b:left_columns == 1
    setlocal nonumber norelativenumber signcolumn=no foldcolumn=0
    let b:left_columns = 0
  else
    setlocal number relativenumber signcolumn=auto foldcolumn=1
    let b:left_columns = 1
  endif
endfunction
" }}}

function! s:syntax_options() abort " {{{
  if exists('g:syntax_on')
    syntax off | hi ColorColumn NONE
  else
    syntax enable
  endif
endfunction
" }}}

" }}}

" Mappings: {{{2
call s:map('n', '[ob', ':set background=light<CR>')
call s:map('n', ']ob', ':set background=dark<CR>')
call s:map('n', 'yob',
      \ ':set bg=<C-R>=&background == "dark" ? "light" : "dark"<CR><CR>')

call s:option_map('_', 'cursorline', 'setlocal')
call s:option_map('<Bar>', 'cursorcolumn', 'setlocal')
call s:map('n', '[ox', ':set cursorline cursorcolumn<CR>')
call s:map('n', ']ox', ':set nocursorline nocursorcolumn<CR>')
call s:map('n', 'yox', ':set <C-R>=<SID>cursor_options()<CR><CR>')

call s:map('n', '[od', ':diffthis<CR>')
call s:map('n', ']od', ':diffoff<CR>')
call s:map('n', 'yod', ':<C-R>=&diff ? "diffoff" : "diffthis"<CR><CR>')

call s:option_map('h', 'hlsearch', 'set')
call s:option_map('i', 'ignorecase', 'set')
call s:option_map('l', 'list', 'setlocal')
call s:option_map('n', 'number', 'setlocal')
call s:option_map('r', 'relativenumber', 'setlocal')
call s:option_map('s', 'spell', 'setlocal')
call s:option_map('w', 'wrap', 'setlocal')

call s:map('n', '[ov', ':set virtualedit+=all<CR>')
call s:map('n', ']ov', ':set virtualedit-=all<CR>')
call s:map('n', 'yov',
      \ ':set <C-R>=(&virtualedit =~# "all") ? "ve=all" : "ve+=all"<CR><CR>')

call s:map('n', '[oc', ':set listchars+=space:␣<CR>')
call s:map('n', ']oc', ':set listchars-=space:␣<CR>')
call s:map('n', 'yoc',
      \ ':set <C-R>=(&listchars =~# "␣") ? "lcs-=space:␣" : "lcs+=space:␣"<CR><CR>')

nnoremap <silent> <Plug>unimpairedToggleWordProcessing :<C-U>call <SID>word_processing_options()<CR>
call s:map('n', '[op', ':setlocal nowrap nolinebreak list textwidth=80<CR>')
call s:map('n', ']op', ':setlocal wrap linebreak nolist textwidth=0<CR>')
call s:map('n', 'yop', '<Plug>unimpairedToggleWordProcessing')

nnoremap <silent> <Plug>unimpairedToggleLeftColumns
      \ :<C-U>call <SID>left_columns_options()<CR>
call s:map('n', '[og',
      \ ':setlocal number relativenumber signcolumn=auto foldcolumn=1<CR>')
call s:map('n', ']og',
      \ ':setlocal nonumber norelativenumber signcolumn=no foldcolumn=0 <CR>')
call s:map('n', 'yog', '<Plug>unimpairedToggleLeftColumns')

nnoremap <silent> <Plug>unimpairedToggleSyntax
      \ :<C-U>call <SID>syntax_options()<CR>
call s:map('n', '[oe', ':syntax enable<CR>')
call s:map('n', ']oe', ':syntax off <Bar> hi ColorColumn NONE<CR>')
call s:map('n', 'yoe', '<Plug>unimpairedToggleSyntax')

" }}}

" }}}

" Activation: {{{1

call s:maps()

" }}}

" vim:set et sw=2 ts=2 tw=78:
