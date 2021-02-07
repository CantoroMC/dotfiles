let g:rainbow#max_level = 16
let g:rainbow#pairs = [ ['(', ')'], ['[', ']'], ['{', '}'] ]

augroup rainbow_lisp
  autocmd!
  autocmd FileType 
        \ vim,tex,zsh,sh,bash,c,cpp,haskell,lua,lhaskell,lisp,clojure,scheme
        \ RainbowParentheses
augroup END
