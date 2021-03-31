function! airline#extensions#tabline#formatters#webDevIcons#format(bufnr, buffers) abort
  let originalFormatter = airline#extensions#tabline#formatters#{g:_webDevIcons_basefmt}#format(a:bufnr, a:buffers)
  return v:lua.webDevIcons(bufname(a:bufnr)) . ' ' . originalFormatter
endfunction
