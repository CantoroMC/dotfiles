" Window Resizing
noremap <silent> <S-Left>
      \ :<C-U>execute v:count1.'wincmd <'<CR>
noremap <silent> <S-Right>
      \ :<C-U>execute v:count1.'wincmd >'<CR>
noremap <silent> <S-Up>
      \ :<C-U>execute v:count1.'wincmd +'<CR>
noremap <silent> <S-Down>
      \ :<C-U>execute v:count1.'wincmd -'<CR>
cnoremap wbd write <Bar> bdelete

" Tabs: {{{1

nnoremap <silent> <C-W>t     :<C-u>tabnew<CR>
nnoremap <silent> <C-W><C-T> :<C-u>tabnew<CR>
nnoremap <silent> <C-W>Q     :<C-u>tabclose<CR>
nnoremap <silent> <C-N>      gt
nnoremap <silent> <C-P>      gT
" Tab version of gf
nnoremap <silent> gtf        <C-W>gf
" Tab version of gF
nnoremap <silent> gtF        <C-W>gF
" Tab version `<C-]>`.
nnoremap <C-]><C-T> <C-W><C-]><C-W>T

" }}}


" Section: Editing

" Some Editing Tricks: {{{1

" Substitutions with ease.
nnoremap <Leader>ra :%s///g<Left><Left><Left>
" Swap two words.
nnoremap <silent> <Leader>sw "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>
" Overwrite the current line with yanked text.
nnoremap <silent> go  pk"_dd
" Indent continuously.
vnoremap < <gv
vnoremap > >gv
" Moving lines up and down.
vnoremap K :move '<-2<CR>gv=gv
vnoremap J :move '>+1<CR>gv=gv
" Assist input normal command on visual mode.
vnoremap n :normal<Space>
" Search something in the current visual range only.
vnoremap / <Esc>/\%V
" Global substitution in visual mode.
vnoremap <C-S> "hy:%s/\V<C-R>h//g<left><left>

" }}}

" Yank And Paste: {{{1

function! s:paste_with_register(register, paste_type, paste_cmd) abort " {{{2
  let l:reg_type = getregtype(a:register)
  let l:store    = getreg(a:register)
  call setreg(a:register, l:store, a:paste_type)
  exe 'normal! "'.a:register.a:paste_cmd
  call setreg(a:register, l:store, l:reg_type)
endfunction
" }}}

function! s:yank_to_plus_operator(type, ...) abort " {{{2
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
" }}}

" Paste To Plus Register: {{{2
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
" }}}

" Copy To Plus Register: {{{2
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
" }}}

" }}}

" vim:fdm=marker
