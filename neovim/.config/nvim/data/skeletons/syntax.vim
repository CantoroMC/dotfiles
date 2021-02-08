" syntax/

" Section: Syntax Guard
if exists('b:current_syntax')
  finish
endif
let b:current_syntax = ''

" Section: Intro
syntax clear
