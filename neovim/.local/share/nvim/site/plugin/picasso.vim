" plugin/picasso.vim
if exists('g:loaded_picasso')
  finish
endif
let g:loaded_picasso = 1

command! Dark
      \ lua require'mc.plugin.picasso'.paint('dark')
command! Light
      \ lua require'mc.plugin.picasso'.paint('light')

command! -nargs=* -complete=custom,s:comp_picasso Picasso
      \ lua require'mc.plugin.picasso'.paint(<f-args>)

function! s:comp_picasso(A, L, P)
  if a:L ==# 'Picasso '.a:A
    return join(['dark', 'light'], "\n")."\n"
  else
    let bg = split(a:L)[1]
    let themes = join(luaeval('require"mc.plugin.picasso".list(_A)', bg), "\n")."\n"
    return themes
  endif
endfunction
