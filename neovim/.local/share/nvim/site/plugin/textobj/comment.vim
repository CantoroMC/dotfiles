" textobj-comment - Text objects for comments

" Section: Plugin Guards
if exists('g:loaded_textobj_comment')
  finish
endif
let g:loaded_textobj_comment = 1


" Section: Text Object Wrapping
call textobj#user#plugin('comment', {
     \ '-': {
     \  'select-a-function': 'textobj#comment#select_a',
     \  'select-a': 'ac',
     \  'select-i-function': 'textobj#comment#select_i',
     \  'select-i': 'ic',
     \  },
     \ 'big': {
     \  'select-a-function': 'textobj#comment#select_big_a',
     \  'select-a': 'aC',
     \  }
     \ })
