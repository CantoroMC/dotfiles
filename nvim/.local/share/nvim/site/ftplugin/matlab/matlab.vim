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

" Compiler: {{{1

" Options And Variables: {{{2
if !exists('g:mlint_path_to_mlint')
  let g:mlint_path_to_mlint='mlint'
endif
let b:mlintTempDir = tempname() . '/'
execute 'compiler '.g:mlint_path_to_mlint
" }}}

" Commands And Mappings: {{{2
command! -buffer MlintRun     :call s:RunLint()
command! -buffer MlintOutline :call s:Outline()

nnoremap <silent> <Plug>MlintRunLint :MlintRun<CR>
nnoremap <silent> <Plug>MlintOutline :MlintOutline<CR>

if !hasmapto('<Plug>MlintRunLint') || !maparg('<LocalLeader>ml','n')
  nmap <buffer> <LocalLeader>ml <Plug>MlintRunLint
endif

if !hasmapto('<Plug>MlintOutline') || !maparg('<LocalLeader>mo','n')
  nmap <buffer> <LocalLeader>mo <Plug>MlintOutline
endif
" }}}

" Functions: {{{2

" Script Identifier: {{{3
function! s:SID() abort
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
" }}}

" Buffer Changed: {{{3
function s:BufChanged()
  if !exists('b:mlint_oldchangedtick')
    let b:mlint_oldchangedtick = b:changedtick
    return 1
  else
    let l:oct = b:mlint_oldchangedtick
    let b:mlint_oldchangedtick = b:changedtick
    return l:oct != b:changedtick
  endif
endfunction
" }}}

" Run Linter: {{{3
function s:RunLint()
  let b:mlint_oldchangedtick = b:changedtick

  " Clear previous matches
  if exists('b:cleared')
    if b:cleared == 0
      silent call s:ClearLint()
      let b:cleared = 1
    endif
  else
    let b:cleared = 1
  endif

  if !isdirectory(b:mlintTempDir)
    call mkdir(b:mlintTempDir)
  endif

  let l:filename = expand('%:t')
  exe 'silent write! ' . fnameescape(b:mlintTempDir . l:filename)
  let l:mlintCommand = shellescape(g:mlint_path_to_mlint). ' ' . shellescape(b:mlintTempDir . l:filename)
  let l:lint         = system(l:mlintCommand)

  " Split the output from mlint and loop over each message
  let l:lint_lines = split(l:lint, '\n')
  highlight MLint term=underline gui=undercurl guisp=Orange
  let b:matched = []
  for l:line in l:lint_lines
    let l:matchDict = {}
    let l:lineNum   = matchstr(l:line, 'L \zs[0-9]\+')
    let l:colStart  = matchstr(l:line, 'C \zs[0-9]\+')
    let l:colEnd    = matchstr(l:line, 'C [0-9]\+-\zs[0-9]\+')
    let l:message   = matchstr(l:line, ': \zs.*')

    if l:lineNum > line('$')
      let l:mID     = matchadd('MLint', '\%'.line('$').'l', '\%>1c')
      let l:lineNum = l:lineNum - 1
    elseif l:lineNum == 0
      echohl WarningMsg
      echo l:message
      echohl None
      let l:mID = 0
    elseif l:colStart > 0

      if l:colEnd > 0
        let l:colStart = l:colStart -1
        let l:colEnd = l:colEnd + 1
        let l:mID = matchadd('MLint', '\%'.l:lineNum.'l'.'\%>'.
              \ l:colStart.'c'.'\%<'.l:colEnd.'c')
      else
        let l:colEnd = l:colStart + 1
        let l:colStart = l:colStart - 1
        let l:mID = matchadd('MLint', '\%'.l:lineNum.'l'.'\%>'.
              \ l:colStart.'c'.'\%<'.l:colEnd.'c')
      endif

    else
      let l:mID = matchadd('MLint', '\%'.l:lineNum.'l','\%>1c')
    endif

    " Define the current buffer number for the quickfix list
    let l:matchDict['bufnr'] = bufnr('')
    let l:matchDict['mID']   = l:mID
    let l:matchDict['lnum']  = l:lineNum
    " Column number for quickfix list
    let l:matchDict['col']      = l:colStart
    let l:matchDict['colStart'] = l:colStart
    let l:matchDict['colEnd']   = l:colEnd
    let l:matchDict['text']     = l:message
    call add(b:matched, l:matchDict)
  endfor
  let b:cleared = 0
endfunction
" }}}

" Get Lint Message: {{{3
function s:GetLintMessage()
  let l:cursorPos = getpos('.')
  for l:lintMatch in b:matched
    " If we're on a line with a match then show the mlint message
    if l:lintMatch['lnum'] == l:cursorPos[1]
      " The two lines commented below cause a message to be shown
      " only when the cursor is actually over the offending item in
      " the line.
            "\ && l:cursorPos[2] > l:lintMatch['colStart']
            "\ && l:cursorPos[2] < l:lintMatch['colEnd']
      echo l:lintMatch['text']
    elseif l:lintMatch['lnum'] == 0
      echohl WarningMsg
      echo l:lintMatch['text']
      echohl None
    endif
  endfor
endfunction
" }}}

" Outline: {{{3
function s:Outline()
  silent call s:RunLint()
  call setqflist(b:matched)
  cwindow
endfunction
" }}}

" Clear Lint Highlighting: {{{3
function s:ClearLint()
  let l:matches = getmatches()
  for l:matchId in l:matches
    if l:matchId['group'] ==? 'MLint'
      call matchdelete(l:matchId['id'])
    end
  endfor
  let b:matched = []
  let b:cleared = 1
endfunction
" }}}

" Clean Up Lint Output: {{{3
function s:Cleanup(filename, mlintTempDir)
  let l:mlintTempDir = a:mlintTempDir

  " Check to prevent multiple calls.
  if !exists('l:lastMlintCleanup') || l:lastMlintCleanup != l:mlintTempDir.a:filename
    let l:lastMlintCleanup = l:mlintTempDir.a:filename

    if isdirectory(l:mlintTempDir)
      if filewritable(l:mlintTempDir.a:filename) == 1
        if delete(l:mlintTempDir.a:filename) == 0
          call delete(l:mlintTempDir.a:filename.'~')
          call delete(l:mlintTempDir,'d')

          if isdirectory(l:mlintTempDir)
            echohl WarningMsg
            echomsg 'mlint: could not delete temp directory ' .
                  \ fnameescape(l:mlintTempDir) .
                  \ '; directory still exists after deletion'
            echohl None
          endif
        else
          echohl WarningMsg
          echomsg 'mlint: could not delete temp file '.
                \ fnameescape(l:mlintTempDir.a:filename).
                \ '; error during file deletion'
          echohl None
        endif
      else
        echohl WarningMsg
        echomsg 'mlint: could not delete temp file '.
              \ fnameescape(l:mlintTempDir.a:filename).
              \ '; no write privileges'
        echohl None
      endif
    endif
  endif
endfunction
" }}}

" }}}

" Auto Commands: {{{2
augroup mlint_msg
  autocmd!
  autocmd CursorHold,CursorHoldI <buffer> if s:BufChanged() | call s:RunLint() | endif
  autocmd CursorHold             <buffer> call s:GetLintMessage()
  autocmd BufWinLeave            <buffer> call s:ClearLint()
  autocmd BufEnter,InsertLeave   <buffer> call s:RunLint()
  autocmd BufUnload              <buffer>
        \ call s:Cleanup(expand("<afile>:t"), getbufvar(expand("<afile>"), "mlintTempDir"))
augroup END
" }}}

" }}}

" Options Restoring: {{{1
let b:undo_ftplugin = 'setlocal suffixesadd< suffixes< '
      \ . ' | setlocal comments< commentstring< '
      \ . '| setlocal foldmethod< foldexpr< '
      \ . '| setlocal nospell<'
      \ . '| unlet! b:match_words b:match_ignorecase '
      \ . '| unlet! b:mlintTempDir b:mlint_oldchangedtick b:changedtick '
      \ . '| unlet! b:cleared b:matched'
      \ . 'call <SNR>' . s:SID() . 'Cleanup(' . '"' . expand('<afile>:t') . '", "'
      \ . getbufvar(expand('<afile>'), 'mlintTempDir') . '")'

" }}}

" vim:fen:fdm=marker:fdl=0:ts=2:sts=2:et:sw=2:sr
