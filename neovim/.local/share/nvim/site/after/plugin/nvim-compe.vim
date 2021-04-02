inoremap <silent> <expr> <Plug>CustomCompeCR
      \ pumvisible() ?
      \   compe#confirm('\<CR>') :
      \ "\<C-g>u\<CR>\<C-r>=EndwiseDiscretionary()\<CR>"
imap <CR> <Plug>CustomCompeCR

inoremap <silent> <expr> <C-x>
      \ pumvisible() ?
      \   compe#close('<C-x>') :
      \   "<C-x>"
