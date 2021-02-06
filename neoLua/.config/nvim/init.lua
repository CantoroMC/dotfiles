-- NeoLua Init File

require('globals.init')

require('user.options')
require('user.keymap')

require('plugin.init')
require('plugin.configs.nvim-tree')
Cmd('filetype plugin indent on')

Cmd('colorscheme ayu')
vim.g.ayu_dark_flavor = 'dark'
