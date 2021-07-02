-- NeoLua Init File
require('mc.util.globals')


-- SETTINGS AND MAPPINGS
require('mc.util.options')
vim.cmd(string.format('source %s/keymap.vim',   vim.fn.stdpath('config')))

-- PLUGINS SETTINGS
require'mc.plugin.config'.digest()

-- CLOSURE
vim.o.secure = true
