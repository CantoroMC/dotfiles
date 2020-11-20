" autoload/tex/compilation.vim

" Auto Loading Guards: {{{1

if exists('g:autoloaded_compilation')
  finish
endif
let g:autoloaded_compilation = 1

" }}}

" Auto Loading Functions: {{{1

function! tex#compile#compiler(compiler) abort " {{{2
  cexpr system(a:compiler.' '.g:tex_compOpt.' '.shellescape(expand('%:p')))
  if s:biblio_command() !=# ''
    call s:biblio_msg()
    caddexpr system(s:biblio_command())
    call system(a:compiler.' '.g:tex_compOpt.' '.shellescape(expand('%:p')))
    caddexpr system(a:compiler.' '.g:tex_compOpt.' '.shellescape(expand('%:p')))
  endif
  call setqflist([], 'r', {'title': toupper(a:compiler)})
  redraw!
endfunction
" }}}

function! tex#compile#delAux() abort " {{{2
  let l:auxs = globpath(expand('%:p:h'), expand('%:r').g:tex_auxRegex, 1, 1)
  for aux in l:auxs
    call delete(fnameescape(aux))
  endfor
endfunction
" }}}

function! tex#compile#dvi2ps() abort " {{{2
  execute '!dvips '.expand('%:p:r').'.dvi'
endfunction
" }}}

function! tex#compile#dvi2pdf() abort " {{{2
  execute '!dvipdf '.expand('%:p:r').'.dvi'
endfunction
" }}}

function! tex#compile#viewDvi() abort " {{{2
  execute '!xdg-open '.expand('%:p:r').'.dvi & disown'
  redraw!
endfunction
" }}}

function! tex#compile#viewPs() abort " {{{2
  execute '!xdg-open '.expand('%:p:r').'.ps & disown'
  redraw!
endfunction
" }}}

function! tex#compile#viewPdf() abort " {{{2
  execute '!xdg-open '.expand('%:p:r').'.pdf & disown'
  redraw!
endfunction
" }}}

" }}}

" Script Functions: {{{1

function! s:biblio_command() abort " {{{2
  let bib_cmd = search('\\bibliographystyle','cnw') ?
        \ 'bibtex '.shellescape(expand('%:r').'.aux') : ''
  if bib_cmd ==# ''
    let bib_cmd = search('biblatex','cnw') ?
          \ 'biber '.shellescape(expand('%:r')) : ''
  endif
  return bib_cmd
endfunction
" }}}

function! s:biblio_msg() abort " {{{2
  echohl MoreMsg
  echomsg 'Resolving bibliography dependencies'
  echohl None
endfunction
" }}}

" }}}
