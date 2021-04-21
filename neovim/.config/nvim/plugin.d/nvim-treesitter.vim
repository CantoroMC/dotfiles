" Section: Foldings -- Are very unstable
" augroup folding_treesitter
"   autocmd!
"   autocmd FileType
"         \ bash,c,cpp,go,lua,nix,python,ruby,tex
"         \ setl foldmethod=expr
"   autocmd FileType
"         \ bash,c,cpp,go,lua,nix,python,ruby,tex
"         \ setl foldexpr=nvim_treesitter#foldexpr()
" augroup END
