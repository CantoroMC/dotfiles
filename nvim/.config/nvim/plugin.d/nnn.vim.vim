let g:nnn#layout = { 'left' : '~20%' }
let g:nnn#set_default_mappings = 0
let g:nnn#action = {
      \ '<c-t>': 'tab split',
      \ '<c-s>': 'split',
      \ '<c-v>': 'vsplit' }

nnoremap <silent> <F2> :NnnPicker %:p:h <CR>
