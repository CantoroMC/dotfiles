let g:tagbar_width            = 28
let g:tagbar_autofocus        = 1
let g:tagbar_compact          = 1
let g:tagbar_show_linenumbers = 2
let g:tagbar_foldlevel        = 2
let g:tagbar_autoshowtag      = 1
let g:tagbar_scopestrs        = {
      \ 'class': "\uf0e8",
      \ 'const': "\uf8ff",
      \ 'constant': "\uf8ff",
      \ 'enum': "\uf702",
      \ 'field': "\uf30b",
      \ 'func': "\uf794",
      \ 'function': "\uf794",
      \ 'getter': "\ufab6",
      \ 'implementation': "\uf776",
      \ 'interface': "\uf7fe",
      \ 'map': "\ufb44",
      \ 'member': "\uf02b",
      \ 'method': "\uf6a6",
      \ 'setter': "\uf7a9",
      \ 'variable': "\uf71b",
      \ }

nnoremap <silent> <F3> :<C-u>TagbarToggle<CR>
