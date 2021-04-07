" autoload/fooldme.vim

if exists('g:autoloaded_fooldme')
  finish
endif
let g:autoloaded_fooldme = 1


function! fooldme#foldMark(to_open) abort
  let [fo,fc] = split(&foldmarker, ',')
  let lnum = line('.')
  let line = getline(lnum)

  if a:to_open
    call setline(lnum, line.' '.fo)
  else
    call setline(lnum, line.fc)
  endif
  Commentary
  if a:to_open
    silent! call repeat#set("\<Plug>FooldmeFMOpen")
  else
    silent! call repeat#set("\<Plug>FooldmeFMClose")
  endif
endfunction

function! s:FoldLvlMax() abort
  return max(map(range(1,line('$')),
        \ {arg1, arg2 -> foldlevel(arg2) >= foldlevel(arg1) ? foldlevel(arg2) : 0}))
endfunction

function! fooldme#orgCycle() abort
  if &foldlevel isnot s:FoldLvlMax()
    set foldlevel+=1
    echohl MoreMsg
    echo 'Fold level = '.&foldlevel
    echohl None
  else
    set foldlevel=0
    echohl MoreMsg
    echo 'Folding overview'
    echohl None
  endif
endfunction

function! fooldme#foldtext() abort
  let l:fold_char = '»'

  let l:start_line = getline(v:foldstart)
  let l:fold_width = v:foldend - v:foldstart + 1
  let l:fold_dashes = repeat(l:fold_char, v:foldlevel)

  let l:indent = indent(v:foldstart)
  let l:fold_indent = repeat(' ',
        \   max( [indent - strdisplaywidth(l:fold_dashes),
        \         strdisplaywidth(l:fold_char)] )
        \ )

  " Strip foldmarkers and commentstring
  let l:strip_regex = '\%(\s*'.
        \ substitute(&commentstring, '\s*%s\s*', '', '') .
        \ '*\s*{{{\d*\s*\)\|\%(\s*{{{\d*\s*\)\|\%(^\s*' .
        \ substitute(&commentstring, '\s*%s\s*', '', '') .
        \ '*\s*\)'
  let l:start_line = substitute(l:start_line, strip_regex, '', 'g')
  " Strip leading and trailing whitespaces
  let l:start_line = substitute(l:start_line, '^\s*\|\s*$', '', 'g')

  let l:right_padding = repeat(' ', 39 - len(l:start_line))


  return printf("%s%s%s%s [%dℓ] ",
        \   l:fold_dashes,
        \   l:fold_indent,
        \   l:start_line,
        \   l:right_padding,
        \   l:fold_width,
        \ )
endfunction
