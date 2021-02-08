" Section: Foldings
augroup folding_treesitter
  autocmd!
  autocmd FileType
        \ bash,c,cpp,go,lua,nix,python,ruby
        \ setl foldmethod=expr
  autocmd FileType
        \ bash,c,cpp,go,lua,nix,python,ruby
        \ setl foldexpr=nvim_treesitter#foldexpr()
augroup END
