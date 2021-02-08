command! PackerDownload packadd! packer.nvim  | lua require('plugin.plugins')
command! PackerInstall  packadd! packer.nvim  | lua require('plugin.plugins').install()
command! PackerUpdate   packadd! packer.nvim  | lua require('plugin.plugins').update()
command! PackerSync     packadd! packer.nvim  | lua require('plugin.plugins').sync()
command! PackerClean    packadd! packer.nvim  | lua require('plugin.plugins').clean()
command! PackerCompile  packadd! packer.nvim  | lua require('plugin.plugins').compile()