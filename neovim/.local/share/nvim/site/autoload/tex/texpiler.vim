" autoload/tex/texpiler.vim

if exists('g:autoloaded_texpiler')
  finish
endif
let g:autoloaded_texpiler = 1


function! s:bibtex_cmd() abort
  let bib_cmd = search('\\bibliographystyle','cnw') ?
        \ 'bibtex '.shellescape(expand('%:r').'.aux') : ''
  if bib_cmd ==# ''
    let bib_cmd = search('biblatex','cnw') ?
          \ 'biber '.shellescape(expand('%:r')) : ''
  endif
  return bib_cmd
endfunction

function! s:biblio_msg() abort
  echohl MoreMsg
  echomsg 'Resolving bibliography dependencies'
  echohl None
endfunction


" Section: Compiler
let s:compile_opts = join(get(g:, 'texpiler_opts',
      \   [ ' -shell-escape',
      \     ' -synctex=1',
      \     ' -interaction=nonstopmode',
      \     ' -file-line-error',
      \     ' --output-directory='.expand('%:p:h')
      \   ]
      \ ), '')

function! tex#texpiler#build(compiler) abort
  cexpr system(a:compiler.' '.s:compile_opts.' '.shellescape(expand('%:p')))
  if s:bibtex_cmd() !=# ''
    call s:biblio_msg()
    caddexpr system(s:bibtex_cmd())
    call system(a:compiler.' '.s:compile_opts.' '.shellescape(expand('%:p')))
    caddexpr system(a:compiler.' '.s:compile_opts.' '.shellescape(expand('%:p')))
  endif
  call setqflist([], 'r', {'title': toupper(a:compiler)})
  redraw!
endfunction



" Section: Open build

function! tex#texpiler#openDvi() abort
  execute '!xdg-open '.expand('%:p:r').'.dvi & disown'
  redraw!
endfunction
function! tex#texpiler#openPs() abort
  execute '!xdg-open '.expand('%:p:r').'.ps & disown'
  redraw!
endfunction
function! tex#texpiler#openPdf() abort
  execute '!xdg-open '.expand('%:p:r').'.pdf & disown'
  redraw!
endfunction


" Section: Delete Compilation Auxiliaries

let s:aux_regex = get(g:,'texpiler_aux_regex',
      \ '*.\(aux\|bak\|swp\|synctex(busy)\|synctex\.gz\|out\|idx\|ind\|ilg\|log\|lof\|lot\|lol\|spl\|toc\|blg\|bbl\|bcf\|run\.xml\|fls\|nav\|vrb\|snm\|xdv\|fdb_latexmk\|maf\|4tc\|xref\|tmp\|pyc\|pyo\|pyg\|mtc\|mtc0\)'
      \ )

function! tex#texpiler#delAux() abort
  let l:auxs = globpath(expand('%:p:h'), expand('%:r').s:aux_regex, 1, 1)
  for aux in l:auxs
    call delete(fnameescape(aux))
  endfor
endfunction
