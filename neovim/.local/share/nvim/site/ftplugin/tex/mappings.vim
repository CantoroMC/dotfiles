nnoremap <silent> <buffer> <C-l>] :call tex#maps#closeEnv()<CR>

nnoremap <silent> <buffer> <C-l>i i\begin{IEEEeqnarray}{rCl}<Esc>o\end{IEEEeqnarray}<Esc>k0
nnoremap <silent> <buffer> <C-l>o i\begin{IEEEeqnarray*}{rCl}<Esc>o\end{IEEEeqnarray*}<Esc>k0
nnoremap <silent> <buffer> <C-l>b i\\<CR><Right>\><Esc>
nnoremap <silent> <buffer> <C-l>m I\IEEEeqnarraymulticol{3}{l}{<CR><Esc>A}\IEEEnonumber\\*<Esc>jI\quad <Esc>
