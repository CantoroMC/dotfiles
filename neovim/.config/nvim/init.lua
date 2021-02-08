-- NeoLua Init File

-- GLOBAL LUA VARIABLES
-- require('globals')
PP = function(v)
  print(vim.inspect(v))
  return v
end


-- SETTINGS AND MAPPINGS
local vimL_user_directory = string.format(
  '%s/init.d/',
  vim.fn.stdpath('config')
)

vim.cmd('source ' .. vimL_user_directory .. 'settings.vim')
vim.cmd('source ' .. vimL_user_directory .. 'keymap.vim')

vim.cmd('colorscheme ayu')

-- PLUGINS SETTINGS
require'plugin.octopus'.digest()


-- CLOSURE

vim.o.secure = true