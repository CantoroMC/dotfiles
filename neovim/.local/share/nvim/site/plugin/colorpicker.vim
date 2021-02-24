command! Dark
      \ lua require'mc.plugin.colorpicker'.choose('dark')
command! Light
      \ lua require'mc.plugin.colorpicker'.choose('light')

command! -nargs=* -complete=custom,s:comp_ColorScheme ColorScheme
      \ lua require'mc.plugin.colorpicker'.choose(<f-args>)

function! s:comp_ColorScheme(A, L, P)
  if a:L ==# 'ColorScheme '.a:A
    return join(['dark', 'light'], "\n")."\n"
  else
    let bg = split(a:L)[1]
    let themes = join(luaeval('require"mc.plugin.colorpicker".list(_A)', bg), "\n")."\n"
    return themes
  endif
endfunction
