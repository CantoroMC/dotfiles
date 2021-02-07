" ftplugin/matlab/matlab.vim
" Language:     matlab
" Maintainer:   Marco Cantoro
" Last Changed: Jul 20, 20
" Description:  - Provided matchit functionalities
"               - Set comment options and file suffixes
"               - highlight MatLab code in Vim, based on the output from Matlab's
"                 in built mlint function.
" Options: :mlint_path_to_mlint 
"     set this variable to the full path to the mlint executable,
"     if it is not found in your system path.


" Section: Filetype Guards

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1


" Section: Local Options

setlocal comments=:%>,:%
setlocal commentstring=%%s

setlocal suffixesadd=.m,.mat
setlocal suffixes+=.asv

setlocal nospell

" Foldings: {{{1

function! s:IndLvl(lnum) abort " {{{2
  return indent(a:lnum) / &shiftwidth
endfunction
" }}}

function! GetMatlabFold() abort " {{{2
  let l:thisIndLvl = s:IndLvl(v:lnum)

  if getline(v:lnum) =~? '^\s*%%.*$'
    if getline(v:lnum - 1) =~? '^\s*%\s.*$'
      return '='
    else
      return 'a1'
    endif
  elseif getline(v:lnum) =~? '^\s*%.*$'
    return '='
  elseif getline(v:lnum) =~? '\v^\s*$'
    if getline(v:lnum - 1) =~? '^\s*%.*$'
      return s:IndLvl(v:lnum - 1)
    else
      return '-1'
    endif
  else
    return l:thisIndLvl
  endif
endfunction
" }}}

setlocal foldmethod=expr
setlocal foldexpr=GetMatlabFold()

" }}}


" Section: Match It

if exists('loaded_matchit')
  let b:match_ignorecase = 0
  let s:conditionalEnd   = '\([-+{\*\:(\/\[]\s*\)\@<!\<end\>\(\s*[-+}\:\*\/)\]]\)\@!'
  let b:match_words      =
        \ '\<\%(if\|switch\|for\|while\|classdef\|methods\|events\|properties\|enumeration\|try\|function\)\>:\<\%(elseif\|case\|otherwise\|break\|continue\|else\)\>:'.s:conditionalEnd
  unlet s:conditionalEnd
endif


" Section: Options Restoring

let b:undo_ftplugin = 'setlocal suffixesadd< suffixes< '
      \ . ' | setlocal comments< commentstring< '
      \ . '| setlocal foldmethod< foldexpr< '
      \ . '| setlocal nospell<'
      \ . '| unlet! b:match_words b:match_ignorecase '
