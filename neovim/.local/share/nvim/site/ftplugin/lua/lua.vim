if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/lua.vim

nnoremap <buffer> <silent> K <Cmd> lua require'mc.ftplugin.lua.options'.keyword_prg()<CR>
