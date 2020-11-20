" textobj-latex - Text objects for LaTeX code

" Plugin Guard: {{{1
if exists('g:loaded_textobj_latex')
  finish
endif
let g:loaded_textobj_latex = 1
" }}}

" Text Object Wrapping: {{{1
call textobj#user#plugin('latex', {
      \ 'environment'  : {
      \   'pattern'    : ['\\begin{[^}]\+}.*\n\s*', '\n^\s*\\end{[^}]\+}.*$'],
      \   'select-a'   : '<buffer>ae',
      \   'select-i'   : '<buffer>ie',
      \ },
      \ 'display-math' : {
      \   'pattern'    : ['\\\[', '\\\]'],
      \   'select-a'   : '<buffer>adm',
      \   'select-i'   : '<buffer>idm',
      \ },
      \ 'inline-math'   : {
      \   'pattern'    : ['\\(', '\\)'],
      \   'select-a'   : '<buffer>aim',
      \   'select-i'   : '<buffer>iim',
      \ },
      \ 'dollar-math-a': {
      \   'pattern'    : '[$][^$]*[$]',
      \   'select'     : '<buffer>a$',
      \ },
      \ 'dollar-math-i': {
      \   'pattern'    : '[$]\zs[^$]*\ze[$]',
      \   'select'     : '<buffer>i$',
      \ },
      \ 'quote'        : {
      \   'pattern'    : ['`', "'"],
      \   'select-a'   : '<buffer>aq',
      \   'select-i'   : '<buffer>iq',
      \ },
      \ 'double-quote': {
      \   'pattern'   : ['``', "''"],
      \   'select-a'  : '<buffer>aQ',
      \   'select-i'  : '<buffer>iQ',
      \ },
      \ 'left-right'  : {
      \   'pattern'   : [
      \     '\\left\((\|\[\|\\lbrace\|\\langle\|)\|\]\|\\rbrace\|\\rangle\||\|\\|\|\.\)',
      \     '\\right\()\|\]\|\\rbrace\|\\rangle\|(\|\[\|\\lbrace\|\\langle\||\|\\|\|\.\)'
      \   ],
      \   'select-a'   : '<buffer>alr',
      \   'select-i'   : '<buffer>ilr',
      \ },
      \ })
" }}}
