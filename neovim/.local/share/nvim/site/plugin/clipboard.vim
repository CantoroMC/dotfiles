" plugin/clipboard.vim
" File: clipboard.vim
" Author: Marco Cantoro <marco dot cantoro92 at outlook dot it>
" Description: Copy and Paste to the + register
" Last Modified: Lug 08, 21


" Section: Plugin Guards

if exists('g:loaded_clipboard')
  finish
endif
let g:loaded_clipboard = 1



" Section: Auxiliary Functions
function! s:paste_with_register(register, paste_type, paste_cmd) abort
  let l:reg_type = getregtype(a:register)
  let l:store    = getreg(a:register)
  call setreg(a:register, l:store, a:paste_type)
  exe 'normal! "'.a:register.a:paste_cmd
  call setreg(a:register, l:store, l:reg_type)
endfunction

function! s:yank_to_plus_operator(type, ...) abort
  let l:sel_save = &selection
  let &selection = 'inclusive'
  let l:reg_save = @@

  if a:0
    silent exe 'normal! `<'.a:type.'`>"+y'
  elseif a:type ==# 'line'
    silent exe 'normal! `[V`]"+y'
  elseif a:type ==# 'block'
    silent exe 'normal! `[\<C-V>`]"+y'
  elseif a:type ==# 'char'
    silent exe 'normal! `[v`]"+y'
  endif

  let &selection = l:sel_save
  let @@         = l:reg_save
endfunction



" Section: Paste To Plus Register:
" Character Wise:
nnoremap <silent> <Leader>p
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'p')<CR>
nnoremap <silent> <Leader>P
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'P')<CR>
nnoremap <silent> <Leader>]p
      \ :<C-U>call <SID>paste_with_register('+', 'c', ']p')<CR>
nnoremap <silent> <Leader>]P
      \ :<C-U>call <SID>paste_with_register('+', 'c', ']P')<CR>
nnoremap <silent> <Leader>gp
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'gp')<CR>
nnoremap <silent> <Leader>gP
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'gP')<CR>
" Line Wise:
nnoremap <silent> <Leader>lp
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'p')<CR>
nnoremap <silent> <Leader>lP
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'P')<CR>
nnoremap <silent> <Leader>l]p
      \ :<C-U>call <SID>paste_with_register('+', 'l', ']p')<CR>
nnoremap <silent> <Leader>l]P
      \ :<C-U>call <SID>paste_with_register('+', 'l', ']P')<CR>
nnoremap <silent> <Leader>lgp
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'gp')<CR>
nnoremap <silent> <Leader>lgP
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'gP')<CR>



" Section: Copy To Plus Register:
nnoremap <silent> <Leader>y
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator<CR>g@
nnoremap <silent> <Leader>yy
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator
      \ <Bar> execute 'normal! '.v:count1.'g@_'<CR>
nnoremap <silent> <Leader>Y
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator
      \ <Bar> execute 'normal! 0'.v:count1.'g@_'<CR>
xnoremap <silent> <Leader>y
      \ :<C-U>call <SID>yank_to_plus_operator(visualmode(), 1)<CR>
