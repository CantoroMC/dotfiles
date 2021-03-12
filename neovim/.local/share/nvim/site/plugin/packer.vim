" plugin/packer.vim
if exists('g:loaded_packer')
  finish
endif
let g:loaded_packer = 1

command! PackerDownload
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins')
command! PackerInstall
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins').install()
command! PackerUpdate
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins').update()
command! PackerSync
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins').sync()
command! PackerClean
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins').clean()
command! PackerCompile
      \ packadd! packer.nvim  | lua require('mc.plugin.plugins').compile()
