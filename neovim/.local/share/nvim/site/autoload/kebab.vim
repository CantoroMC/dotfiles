" autoload/kebab.vim

" Auto Loading Guards

if exists('g:autoloaded_kebab')
  finish
endif
let g:autoloaded_kebab = 1


" Section: cding

function! s:HasGitRepo(path) abort
  let is_git = trim(system('git -C '.a:path.' rev-parse --is-inside-work-tree'))
  if is_git ==# 'true'
    return 1
  else
    return 0
  endif
endfunction

function! kebab#rootDir() abort
  let filedir = expand('%:p:h')

  if isdirectory(filedir)
    if s:HasGitRepo(filedir)
      let l:gitdir = trim(system('git -C '.filedir.' rev-parse --show-toplevel'))
      if strlen(l:gitdir) isnot 0
        return l:gitdir
      else
        return filedir
      endif
    else
      return filedir
    endif
  else
    return filedir
  endif
endfunction



" Section: Wrappers

function! kebab#pdf_reader(reader, ...) abort
  if a:0 is 0
    let pdf = fnameescape(expand('%:p:r')).'.pdf'
  elseif a:0 is 1
    let pdf = a:1
  endif

  execute '!'.a:reader.' '.pdf.' & disown'
endfunction

function! kebab#browse(...) abort
  if a:0 is 0
    let name = fnameescape(expand('%:p'))
  elseif a:0 is 1
    let name = a:1
  endif

  execute '!'.$BROWSER.' '.name.' & disown'
endfunction

function! kebab#OpenURLUnderCursor()
  let l:uri = matchstr(getline('.'), '[a-z]*:\/\/[^ >,;()]*')
  " let l:uri = shellescape(l:uri, 1)
  if l:uri != ''
    silent exec "!xdg-open '" . l:uri. "' & disown"
    :redraw!
  endif
endfunction
