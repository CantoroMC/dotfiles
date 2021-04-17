-- NeoLua Init File
require('mc.util.globals')

-- SETTINGS AND MAPPINGS
vim.cmd(string.format('source %s/settings.vim', vim.fn.stdpath('config')))
vim.cmd(string.format('source %s/keymap.vim',   vim.fn.stdpath('config')))

-- PLUGINS SETTINGS
require'mc.plugin.octolua'.digest()

-- CLOSURE
vim.o.secure = true
