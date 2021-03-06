" vim-textobj-parameter - Text objects for function parameter.

if exists('g:loaded_textobj_parameter')
  finish
endif
let g:loaded_textobj_parameter = 1


call textobj#user#plugin('parameter', {
      \   '-': {
      \     'select-a'           : 'aP',
      \     '*select-a-function*': 'textobj#parameter#select_a',
      \     'select-i'           : 'iP',
      \     '*select-i-function*': 'textobj#parameter#select_i'
      \   }
      \ , 'greedy': {
      \     'select-i': 'iPP',
      \     '*select-i-function*': 'textobj#parameter#select_greedy_i'
      \   }
      \ })
