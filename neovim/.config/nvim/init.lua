-- NeoLua Init File
require('mc.util.globals')

-- SETTINGS AND MAPPINGS
require('mc.util.options')
require('mc.util.keymap')

-- PLUGINS SETTINGS
require'mc.plugin.config'.digest()

-- CLOSURE
vim.o.secure = true
