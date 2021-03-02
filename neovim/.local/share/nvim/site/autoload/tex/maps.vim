function! tex#maps#closeEnv() abort
  let l:currPos = getpos('.')

  call matchit#MultiMatch("bW", "n")
  let env = matchstr(matchstr(getline('.'), '\\begin\s*\({\a\+\*\=}\)'),
        \ '\({\a\+\*\=}\)')[1:-2]

  call setpos('.', l:currPos)
  execute 'normal! o\end{'.env.'}'
endfunction
