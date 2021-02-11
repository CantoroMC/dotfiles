if exists('g:loaded_virgilio')
  finish
endif
let g:loaded_virgilio = 1


" Section: Channels list
let s:virgilio_channels = get(g:, 'virgilio_channels',
      \   [ 'buffer',
      \     'qfl',
      \   ]
      \ )


" Section: User Interface
function! s:comp_Outputter(A, L, P) abort
  return join(s:virgilio_channels, "\n")."\n"
endfunction

command! -nargs=? -complete=custom,s:comp_Outputter Virgilio
      \ :call virgilio#lookup(<f-args>)
nnoremap <silent> <Leader>K :<C-u>Virgilio<CR>
