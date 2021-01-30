" Vim Filetype Plugin: {{{1
" Language:     tex,latex
" Maintainer:   Marco Cantoro
" Last Changed: Jul 26, 20
" }}}

" Filetype Guards: {{{1
if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/tex.vim
" }}}

" Global Variables: {{{1

if !exists('g:tex_compOpt')
  let g:tex_compOpt = '--output-directory='.expand('%:p:h').' -shell-escape -synctex=1 -interaction=nonstopmode -file-line-error'
endif

if !exists('g:tex_auxRegex')
  let g:tex_auxRegex = '*.\(aux\|bak\|swp\|synctex(busy)\|synctex\.gz\|out\|idx\|ind\|ilg\|log\|lof\|lot\|lol\|spl\|toc\|blg\|bbl\|bcf\|run\.xml\|fls\|nav\|vrb\|snm\|xdv\|fdb_latexmk\|maf\|4tc\|xref\|tmp\|pyc\|pyo\|pyg\|mtc\|mtc0\)'
endif

" }}}

" Local Options: {{{
" TODO: smarter path
setlocal path+=$TEXMFDIST/**,$TEXMFHOME/bibtex/**,$TEXMFHOME/tex/latex/**
setlocal suffixesadd+=.cls,.sty,.bib,.bst
setlocal errorformat=%f:%l:\ %m,%f:%l-%\\d%\\+:\ %m
" }}}

" Commands And Mappings: {{{1

" Compilers: {{{2

command! -buffer -bang Latex
      \ call tex#compile#compiler('latex') |
      \ if <bang>0 |
      \   call tex#compile#viewDvi() |
      \ endif |
command! -buffer -bang Pslatex
      \ call tex#compile#compiler('pslatex') |
      \ if <bang>0 |
      \   call tex#compile#viewPs() |
      \ endif |
command! -buffer -bang Pdflatex
      \ call tex#compile#compiler('pdflatex') |
      \ if <bang>0 |
      \   call tex#compile#viewPdf() |
      \ endif |
command! -buffer -bang Xelatex
      \ call tex#compile#compiler('xelatex') |
      \ if <bang>0 |
      \   call tex#compile#viewPdf() |
      \ endif |
command! -buffer -bang Lualatex
      \ call tex#compile#compiler('lualatex') |
      \ if <bang>0 |
      \   call tex#compile#viewPdf() |
      \ endif |

nnoremap <silent> <Plug>TexPdflatex :Pdflatex<CR>
if !hasmapto('<Plug>TexPdflatex') || !maparg('<F8>','n')
  nmap <buffer> <F8> <Plug>TexPdflatex
endif

" }}}

" Auxiliaries: {{{2

command! -buffer TexCleanOutput call tex#compile#delAux()

augroup texTasks
  autocmd!
  autocmd BufDelete,BufUnload *.tex call tex#compile#delAux()
augroup END

" }}}

" Readers: {{{2

if exists(':LLPStartPreview')
  command! -buffer TexPreview LLPStartPreview
  nnoremap <silent> <Plug>TexPreview :TexPreview<CR>
  if !hasmapto('<Plug>TexPreview') || !maparg('<F9>','n')
    nmap <buffer> <F9> <Plug>TexPreview
  endif
endif

command! -buffer TexReadDvi call tex#compile#viewDvi()
command! -buffer TexReadPs  call tex#compile#viewPs()
command! -buffer TexReadPdf call tex#compile#viewPdf()

nnoremap <silent> <Plug>TexReadPdf :TexReadPdf<CR>
if !hasmapto('<Plug>TexReadPdf') || !maparg('<F20>','n')
  nmap <buffer> <F20> <Plug>TexReadPdf
endif
" }}}

" More Commands And Mappings: {{{2
command! -buffer TexCloseEnvironment :normal Yp0fbcwend<Esc>ko
nnoremap <silent> <buffer> <LocalLeader>ce :TexCloseEnvironment<CR>
" }}}

" }}}

" Options Restoring: {{{1
let b:undo_ftplugin .= '| setl pa< sua< efm< '
" }}}
