nnoremap <silent> <buffer> <C-c>] :call tex#maps#closeEnv()<CR>

nnoremap <silent> <buffer> <C-c>i i\begin{IEEEeqnarray}{rCl}<Esc>o\end{IEEEeqnarray}<Esc>k0
nnoremap <silent> <buffer> <C-c>o i\begin{IEEEeqnarray*}{rCl}<Esc>o\end{IEEEeqnarray*}<Esc>k0
nnoremap <silent> <buffer> <C-c>b i\\<CR><Right>\><Esc>
nnoremap <silent> <buffer> <C-c>m I\IEEEeqnarraymulticol{3}{l}{<CR><Esc>A}\IEEEnonumber\\*<Esc>jI\quad <Esc>
