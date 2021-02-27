" Vim Filetype Plugin:
" Language:     tex,latex
" Maintainer:   Marco Cantoro
" Last Changed: Jul 26, 20


" Section: Builder
command! -buffer -bang Latex
      \ call tex#texpiler#build('latex') |
      \ if <bang>0 |
      \   call tex#texpiler#openDvi() |
      \ endif |
command! -buffer -bang Pslatex
      \ call tex#texpiler#build('pslatex') |
      \ if <bang>0 |
      \   call tex#texpiler#openPs() |
      \ endif |
command! -buffer -bang Pdflatex
      \ call tex#texpiler#build('pdflatex') |
      \ if <bang>0 |
      \   call tex#texpiler#openPdf() |
      \ endif |
command! -buffer -bang Xelatex
      \ call tex#texpiler#build('xelatex') |
      \ if <bang>0 |
      \   call tex#texpiler#openPdf() |
      \ endif |
command! -buffer -bang Lualatex
      \ call tex#texpiler#build('lualatex') |
      \ if <bang>0 |
      \   call tex#texpiler#openPdf() |
      \ endif |

nnoremap <silent> <Plug>TexPdflatex :Pdflatex<CR>
if !hasmapto('<Plug>TexPdflatex') || !maparg('<F8>','n')
  nmap <buffer> <F8> <Plug>TexPdflatex
endif



" Section: Open build
command! -buffer TReadDvi call tex#texpiler#openDvi()
command! -buffer TReadPs  call tex#texpiler#openPs()
command! -buffer TReadPdf call tex#texpiler#openPdf()

nnoremap <silent> <Plug>TReadPdf :TReadPdf<CR>
if !hasmapto('<Plug>TReadPdf') || !maparg('<C-c>o','n')
  nmap <buffer> <C-c>o <Plug>TReadPdf
endif



" Section: Delete Compilation Auxiliaries
command! -buffer TexCleanAux call tex#texpiler#delAux()
augroup texTasks
  autocmd!
  autocmd BufDelete,BufUnload *.tex call tex#texpiler#delAux()
augroup END
