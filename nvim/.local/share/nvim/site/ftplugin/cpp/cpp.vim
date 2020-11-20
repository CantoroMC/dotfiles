nnoremap <buffer> <S-F8> :w! <Bar> !compC -bo <C-r>%
nnoremap <buffer> <F20>  :w! <Bar> !compC -bo <C-r>%
nnoremap <buffer> <F8>   :w! <Bar> !compC -bg <C-r>%
nnoremap <buffer> <F5>   :execute '!'.fnameescape(expand('%:p:r'))

" Split Header Or Source: {{{1
command! -nargs=0 SplitHeaderSource call s:splitHeadOrSource()

function! s:splitHeadOrSource() abort
  let l:ext = fnameescape(expand('%:e'))
  let l:fileRoot = fnameescape(expand('%:p:r'))

  if l:ext == 'h' || l:ext == 'hpp'
    if filereadable(l:fileRoot.'.cpp')
      execute 'split '.l:fileRoot.'.cpp | res 30'
    elseif filereadable(l:fileRoot.'.c')
      execute 'split '.l:fileRoot.'.c | res 30'
    else
      return
    endif
  elseif l:ext == 'cpp' || l:ext == 'c'
    if filereadable(l:fileRoot.'.h')
      execute 'split '.l:fileRoot.'.h | res 15'
    elseif filereadable(l:fileRoot.'.hpp')
      execute 'split '.l:fileRoot.'.hpp | res 15'
    else
      return
    endif
  endif
endfunction
" }}}
