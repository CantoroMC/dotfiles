" autoload/amivgo.vim

" Auto Loading Guards: {{{1

if exists('g:autoloaded_amivgo')
  finish
endif
let g:autoloaded_amivgo = 1

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

function! amivgo#eval_base_number(base, exp) abort " {{{
  let l:format = {
        \ 2: '0b%b',
        \ 8: '0o%o',
        \ 10: '%d',
        \ 16: '0x%x'
        \ }

  let result = eval(a:exp)
  if type(0) != type(result)
    echoerr 'The result of the given expression have to be Number'
    return ''
  endif

  return printf(l:format[a:base], result)
endfunction
" }}}

if has('nvim')
  function! amivgo#float_base_numbers(number_text) abort " {{{
    let l:number = str2nr(a:number_text)
    if type(0) != type(l:number)
      echoerr 'The given value '.l:number.' is NOT number.'
      return
    endif

    let l:content = [
          \ printf(' Bin: 0b%b ', l:number),
          \ printf(' Oct: 0o%o', l:number),
          \ printf(' Dec: %d', l:number),
          \ printf(' Hex: 0x%x', l:number),
          \ ]

    let l:buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(l:buf, 0, -1, v:true, l:content)
    call nvim_buf_set_keymap(l:buf, 'n', '<Esc>', '<C-W>q', {})

    let l:opts = {
          \ 'relative': 'win',
          \ 'height'  : len(l:content),
          \ 'width'   : max(map(l:content, {_, c -> len(c)})),
          \ 'row'     : 0,
          \ 'col'     : winwidth(0) - winwidth(bufwinid(l:buf)),
          \ 'style'   : 'minimal'
          \}
    let l:win = nvim_open_win(l:buf, v:true, l:opts)
    call nvim_win_set_option(l:win, 'winhl', 'Normal:Question')
  endfunction
  " }}}
endif

" }}}

function! amivgo#get_char_encoding() abort " {{{1
  redraw
  echohl MoreMsg
  echomsg 'Press any key: '
  echohl None

  let c = getchar()
  while c ==# "\<CursorHold>"
    redraw
    echohl MoreMsg
    echomsg 'Press any key: '
    echohl None
    let c = getchar()
  endwhile

  redraw
  echohl MoreMsg
  echo printf('Raw: "%s" | Char: "%s"', c, nr2char(c))
  echohl None
endfunction
" }}}

function! amivgo#uuid() abort " {{{1
  let r = system('uuidgen')
  let r = substitute(r, '^[\r\n\s]*\|[\r\n\s]*$', '', 'g')
  return r
endfunction
" }}}

" Section: Vim management

function! amivgo#auto_mkdir(dir, force) abort " {{{1
  if empty(a:dir) || a:dir =~# '^\w\+://' || isdirectory(a:dir)
    return
  endif

  if !a:force
    echohl MoreMsg
    call inputsave()

    try
      let result = input(
            \ printf('"%s" does not exist. Create? [y/N]', a:dir),
            \ '',
            \ )
      if result !=? 'y'
        echohl WarningMsg
        echo 'Canceled'
        return
      endif
    finally
      call inputrestore()
      echohl None
    endtry

  endif
  call mkdir(a:dir, 'p')
endfunction
" }}}

function! amivgo#echo_hi_id() abort " {{{1
  echohl MoreMsg
  echomsg 'highlight<'.synIDattr(synID(line('.'), col('.'), 1), 'name').'> '
        \ .'transparent<'.synIDattr(synID(line('.'), col('.'), 0), 'name').'> '
        \ .'link<'.synIDattr(synIDtrans(synID(line('.'), col('.'), 1)),'name').
        \ '>'
  echohl None
endfunction
" }}}

function! amivgo#retrieve_map(...) abort " {{{1
  let s:mode    = input('Choose mapping mode: ', 'n')
  let s:mapping = input('Choose mapping key: ', '<Leader>')

  redraw
  echohl ModeMsg
  echo maparg(s:mapping, s:mode,0,1)
  echohl None
endfunction
" }}}

function! amivgo#clear_registers() abort " {{{1
  let regs=split(
        \ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/-"',
        \ '\zs'
        \ )
  for r in regs
    call setreg(r, [])
  endfor
endfunction
" }}}

" Frequently Used Configuration Files: {{{1
function! amivgo#comp_freq_cfgs(A,L,P) abort
  return join(sort(keys(g:amivgo_freq_cfgs)), "\n")."\n"
endfunction
" }}}

function! amivgo#source_vim_script(path) abort " {{{1
  let path = expand(a:path)
  if !filereadable(path) || getbufvar(a:path, '&filetype') !=# 'vim'
    return
  endif
  execute 'source' fnameescape(path)
  echohl MoreMsg
  echomsg printf(
        \ '"%s" has sourced (%s)',
        \ simplify(fnamemodify(path, ':~:.')),
        \ strftime('%c'),
        \)
  echohl None
endfunction
" }}}

function! amivgo#eval_lines() range abort " {{{1
" TODO: Does not work with lines containing trailing comments
  if getbufvar(expand('%'), '&filetype') !=# 'vim'
    return
  else
    let l:text_lines = getline(a:firstline, a:lastline)
    while match(l:text_lines, '^\"') >= 0
      call remove(l:text_lines, match(l:text_lines, '^"'))
    endwhile

    execute join(l:text_lines, "\n")."\n"
  endif
endfunction
" }}}

" Sub Section: Sessions Management {{{1

function! amivgo#save_session(...) abort " {{{2
  call mkdir(g:session_dir, 'p')
  execute 'mksession!' (a:0 == 0) ? (g:last_session) : (g:session_dir.a:1)
endfunction
" }}}

function! amivgo#comp_session_files(A, L, P) abort " {{{2
  let l:filepaths = split(glob(g:session_dir.'*'), '\n')
  return map(l:filepaths, {_, v -> fnamemodify(v, ':t')})
endfunction
" }}}

" }}}

" Sub Section: View Management {{{1

function! amivgo#delete_view(bang) abort " {{{2
  if &modified && a:bang !=# '!'
    echohl WarningMsg
    echo 'Use bang to forcedly remove view file on modified buffer'
    echohl None
    return
  endif

  let path = substitute(expand('%:p:~'), '=', '==', 'g')
  let path = substitute(path, '/', '=+', 'g') . '='
  let path = printf('%s/%s', &viewdir, path)

  if filewritable(path)
    call delete(path)
    silent edit! %
    echo 'View file has removed: '.path
  endif
endfunction
" }}}

function! amivgo#is_view_available() abort " {{{2
  if &buftype =~# '^\%(help\|nofile\|quickfix\|terminal\)$'
    return 0
  endif
  return &buflisted && filereadable(expand('%'))
endfunction
" }}}

" }}}
