-- NeoLua Init File

-- Settings and Mappings
local vimL_user_directory = string.format(
	'%s/userVimL/',
	vim.fn.stdpath('config')
)

vim.cmd('source ' .. vimL_user_directory .. 'settings.vim')
vim.cmd('source ' .. vimL_user_directory .. 'keymap.vim')


-- require('globals')
require('plugin.octopus')

vim.cmd('colorscheme ayu')
-- vim.g.ayu_dark_flavor = 'dark'

vim.o.secure = true
