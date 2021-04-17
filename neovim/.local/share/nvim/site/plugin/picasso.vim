" plugin/picasso.vim
if exists('g:loaded_picasso')
  finish
endif
let g:loaded_picasso = 1

command! Dark
      \ lua require'mc.plugin.picasso.brush'.paint('dark')
command! Light
      \ lua require'mc.plugin.picasso.brush'.paint('light')

command! -nargs=* -complete=custom,s:comp_ColorScheme ColorScheme
      \ lua require'mc.plugin.picasso.brush'.paint(<f-args>)

function! s:comp_ColorScheme(A, L, P)
  if a:L ==# 'ColorScheme '.a:A
    return join(['dark', 'light'], "\n")."\n"
  else
    let bg = split(a:L)[1]
    let themes = join(luaeval('require"mc.plugin.picasso.brush".list(_A)', bg), "\n")."\n"
    return themes
  endif
endfunction
