" plugin/octolua.vim
if exists('g:loaded_octolua')
  finish
endif
let g:loaded_octolua = 1

command! Octolua
      \ lua require'mc.plugin.octolua'.excrete()
