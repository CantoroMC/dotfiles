-- NeoLua Init File

-- require('globals')
PP = function(v)
  print(vim.inspect(v))
  return v
end

-- SECTION: SETTINGS AND MAPPINGS
local vimL_user_directory = string.format(
  '%s/init.d/',
  vim.fn.stdpath('config')
)

vim.cmd('source ' .. vimL_user_directory .. 'settings.vim')
vim.cmd('source ' .. vimL_user_directory .. 'keymap.vim')


require('plugin.octopus')

vim.cmd('colorscheme jellybeans')
-- vim.g.ayu_dark_flavor = 'dark'

vim.o.secure = true
