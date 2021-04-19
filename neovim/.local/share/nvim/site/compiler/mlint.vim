" Vim Compiler File:

" Compiler:         Matlab mlint code checker
" Maintainer:       Marco Cantoro
" Latest Revision:  Lug 19, 20
" Comments: {{{2    mlint messages format
"                   - L x (C y): message (where x and y are line number and
"                   column number)
"                   - L x (C y-z): message (where x is the line number, y and
"                   z the column numbers where the error comes from) }}}


" Add Mlint Compiler:

if exists('current_compiler')
  finish
endif
let current_compiler = 'mlint'

" Compiler Set:

if exists(':CompilerSet') != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=mlint\ -id\ %

CompilerSet errorformat=
      \%-P==========\ %f\ ==========,
      \%-G%>==========\ %s\ ==========,
      \%-G%>L\ %l\ (C\ %c):\ MDOTM%m,
      \L\ %l\ (C\ %c):\ %m,
      \L\ %l\ (C\ %c-%*[0-9]):\ %m,
      \%-Q
