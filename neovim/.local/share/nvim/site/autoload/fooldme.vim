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
