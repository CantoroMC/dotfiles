if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/cpp.vim


" Section: Termdebug
nnoremap <silent> <buffer> <C-D><C-B> :<C-u>Break<CR>
nnoremap <silent> <buffer> <C-D><C-C> :<C-u>Clear<CR>
nnoremap <silent> <buffer> <C-D><C-R> :<C-u>Run<CR>

nnoremap <silent> <buffer> <C-D><C-S> :<C-u>Step<CR>
nnoremap <silent> <buffer> <C-D><C-N> :<C-u>Over<CR>
nnoremap <silent> <buffer> <C-D><C-F> :<C-u>Finish<CR>
nnoremap <silent> <buffer> <C-D><C-G> :<C-u>Continue<CR>
nnoremap <silent> <buffer> <C-D><C-S> :<C-u>Stop<CR>
