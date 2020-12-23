" Vim Filetype Plugin: {{{

" Language:     matlab
" Maintainer:   Marco Cantoro
" Last Changed: Jul 20, 20
" Description:  - Provided matchit functionalities
"               - Set comment options and file suffixes
"               - highlight MatLab code in Vim, based on the output from Matlab's
"                 in built mlint function.
" Options: {{{2
"   g:mlint_path_to_mlint: set this variable to the full path to the mlint
"                          executable, if it is not found in your system path.
" }}}

" }}}

" Filetype Guards: {{{1

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" }}}

" Local Options: {{{1

setlocal comments=:%>,:%
setlocal commentstring=%%s

setlocal suffixesadd=.m,.mat
setlocal suffixes+=.asv

setlocal foldmethod=expr
setlocal foldexpr=GetMatlabFold()

setlocal nospell

" }}}

" Match It: {{{1

if exists('loaded_matchit')
  let b:match_ignorecase = 0
  let s:conditionalEnd   = '\([-+{\*\:(\/\[]\s*\)\@<!\<end\>\(\s*[-+}\:\*\/)\]]\)\@!'
  let b:match_words      =
        \ '\<\%(if\|switch\|for\|while\|classdef\|methods\|events\|properties\|enumeration\|try\|function\)\>:\<\%(elseif\|case\|otherwise\|break\|continue\|else\)\>:'.s:conditionalEnd
  unlet s:conditionalEnd
endif

" }}}

" Foldings: {{{1

function! GetMatlabFold() abort

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

function! s:IndLvl(lnum) abort
  return indent(a:lnum) / &shiftwidth
endfunction

" }}}

" Options Restoring: {{{1
let b:undo_ftplugin = 'setlocal suffixesadd< suffixes< '
      \ . ' | setlocal comments< commentstring< '
      \ . '| setlocal foldmethod< foldexpr< '
      \ . '| setlocal nospell<'
      \ . '| unlet! b:match_words b:match_ignorecase '
" }}}

" vim:fen:fdm=marker:fdl=0:ts=2:sts=2:et:sw=2:sr
