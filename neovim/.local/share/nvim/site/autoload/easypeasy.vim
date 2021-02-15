" autoload/easypeasy.vim

if exists('g:autoloaded_easypeasy')
  finish
endif
let g:autoloaded_easypeasy = 1


" Section: Views Management

function! easypeasy#delete_view(bang) abort
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

function! easypeasy#can_be_viewed() abort
  if &buftype =~# '^\%(help\|nofile\|quickfix\|terminal\)$'
    return 0
  endif
  return &buflisted && filereadable(expand('%'))
endfunction


" Section: Sessions Management

let s:session_dir      = get(g:, 'session_dir', stdpath('data').'/sessions/')
let s:def_session_name = get(g:, 'def_session_name', 'last.vim')
let s:last_session = s:session_dir.s:def_session_name

function! easypeasy#save_session(...) abort
  if isdirectory(s:session_dir) is 0 
    call mkdir(s:session_dir, 'p')
  end
  execute 'mksession!' (a:0 == 0) ? (s:last_session) : (s:session_dir.a:1)
endfunction

function! easypeasy#comp_session_files(A, L, P) abort
  let l:filepaths = split(glob(s:session_dir.'*'), '\n')
  return map(l:filepaths, {_, v -> fnamemodify(v, ':t')})
endfunction


" Session: AutoMkdir, findMappedAction, echoHiId

function! easypeasy#auto_mkdir(dir, force) abort
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
      if !( result ==? 'y' || result ==? 'yes' )
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

function! easypeasy#retrieve_map() abort " {{{1
  let s:mode    = input('Choose mapping mode: ', 'n')
  let s:mapping = input('Choose mapping key: ', '<Leader>')

  redraw
  echohl ModeMsg
  echo maparg(s:mapping, s:mode,0,1)
  echohl None
endfunction

function! easypeasy#echo_HiId() abort
  echohl MoreMsg
  echomsg 'highlight<'.synIDattr(synID(line('.'), col('.'), 1), 'name').'> '
        \ .'transparent<'.synIDattr(synID(line('.'), col('.'), 0), 'name').'> '
        \ .'link<'.synIDattr(synIDtrans(synID(line('.'), col('.'), 1)),'name').
        \ '>'
  echohl None
endfunction
