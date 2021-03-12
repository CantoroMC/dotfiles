" plugin/lanternate.vim
if exists('g:loaded_lanternate')
  finish
endif
let g:loaded_lanternate = 1

let LuaToggleAlternate = luaeval('require"mc.plugin.lanternate".toggleAlternate')

" Available commands
command! -nargs=* ToggleAlternate call LuaToggleAlternate(expand('<cword>'))
nnoremap <silent> <C-C><C-a> :<C-U>ToggleAlternate<CR>
