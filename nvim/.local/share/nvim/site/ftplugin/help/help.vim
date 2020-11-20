" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/help.vim
" }}}

" Folding: {{{1

function! HelpFold() abort " {{{2
  let line = getline(v:lnum)

  if line =~? '\v^\s*$'
    return '-1'
  endif

  if line =~ '^=\+$'
    return ">1"
  endif

  if line =~ '^-\+$'
    return ">2"
  endif

  return "="
endfunction
" }}}

function! HelpFoldText() abort " {{{2
    let title = getline(v:foldstart + 1)
    let foldsize = printf('%4S',v:foldend - v:foldstart + 1)
    let linecount = '+'.v:folddashes.' '.foldsize.' lines: '
    return linecount.title
endfunction
" }}}

setlocal foldexpr=HelpFold()
setlocal foldtext=HelpFoldText()
setlocal foldmethod=expr

" }}}

" Options Restoring: {{{1
let b:undo_ftplugin .= '| setl fdm< fde< fdt< '
" }}}
