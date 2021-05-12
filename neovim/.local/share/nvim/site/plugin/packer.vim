" plugin/packer.vim
if exists('g:loaded_packer')
  finish
endif
let g:loaded_packer = 1

command! PackerInspect  lua require'mc.plugin.config'.excrete()
command! PackerDownload lua require'mc.plugin.plugins'

command! PackerClean
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').clean()
command! -nargs=* PackerCompile
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').compile(<q-args>)
command! PackerInstall
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').install()
command! PackerProfile
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').profile_output()
command! PackerStatus
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').status()
command! PackerSync
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').sync()
command! PackerUpdate
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').update()
command! -nargs=+ -complete=customlist,v:lua.require'mc.plugin.plugins'.loader_complete PackerLoad
      \ packadd! packer.nvim | lua require('mc.plugin.plugins').loader(<q-args>)
