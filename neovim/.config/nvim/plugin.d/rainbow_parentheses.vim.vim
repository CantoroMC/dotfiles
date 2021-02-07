let g:rainbow#max_level = 16
let g:rainbow#pairs = [ ['(', ')'], ['[', ']'], ['{', '}'] ]

augroup rainbow_lisp
  autocmd!
  autocmd FileType 
        \ vim,tex,bib,zsh,sh,bash,c,cpp,haskell,lhaskell,lua,ruby,go,config,conf,lisp,clojure,scheme,matlab,
        \ RainbowParentheses
augroup END
