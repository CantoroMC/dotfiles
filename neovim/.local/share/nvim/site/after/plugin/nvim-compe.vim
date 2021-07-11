inoremap <silent><expr> <C-Space> compe#complete()

inoremap <silent> <expr> <Plug>CustomCompeCR
      \ pumvisible() ?
      \   compe#confirm('<CR>') :
      \ "\<C-g>u\<CR>\<C-r>=EndwiseDiscretionary()\<CR>"
imap <CR> <Plug>CustomCompeCR

inoremap <silent> <expr> <C-e>
      \ pumvisible() ?
      \   compe#close('<C-e>') :
      \   "<C-e>"

inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
