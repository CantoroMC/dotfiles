-- NeoLua Init File

require('globals')


require('user.options')
require('user.keymap')
require('plugin.octopus')
require('user.afterplug')

vim.cmd('colorscheme ayu')
vim.g.ayu_dark_flavor = 'dark'
