" Vim filetype plugin {{{1
" Language:     Portlable Game Notation
" Author:       Marco Cantoro <marco.cantoro92@outlook.it>
" Filenames:    *.pgn
" }}}

" Filetype Guards: {{{1

if exists('b:did_ftplugin')
  finish
endif

" }}}

" Local Options: {{{1

setlocal nospell
setlocal foldmethod=expr
setlocal foldexpr=PgnFold()
setlocal foldtext=PgnFoldText()

" }}}

" Functions: {{{1

" Folding: {{{2

function! PgnFold() abort " {{{3
  let line = getline(v:lnum)
  if line =~? '\v^\s*$'
    return '0'
  endif
  return '1'
endfunction
" }}}

function! PgnFoldText() abort " {{{3
  if matchstr(getline(v:foldstart), '\v^\[') ==# '['
    let whiteTag = matchstr(getline(v:foldstart, v:foldend), 'White')
    let blackTag = matchstr(getline(v:foldstart, v:foldend), 'Black')
    if matchstr(whiteTag, '\v\,') ==# ',' && matchstr(blackTag, '\v\,') ==# ','
      let whiteName = matchstr(whiteTag, '\v\w*\,\s\w*')
      let blackName = matchstr(blackTag, '\v\w*\,\s\w*')
      let foldsize = (v:foldend - v:foldstart + 1)
      let linecount = '+-- '.foldsize.' lines: '
      return linecount.whiteName.' vs '.blackName
    else
      return foldtext()
    endif
  else
    return foldtext()
  endif
endfunction
" }}}

" }}}

" }}}

" Restoring Options: {{{1

let b:undo_ftplugin = 'setlocal nospell< foldmethod< foldexpr< foldtext< '

" }}}
