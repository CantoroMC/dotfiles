-- NeoLua Init File

-- GLOBAL LUA VARIABLES 
PP = function(v)
  print(vim.inspect(v))
  return v
end


-- SETTINGS AND MAPPINGS
local vimL_user_directory = string.format(
  '%s/init.d/',
  vim.fn.stdpath('config')
)
vim.cmd('source ' .. vim.fn.stdpath('config') .. '/settings.vim')
vim.cmd('source ' .. vim.fn.stdpath('config') .. '/keymap.vim')

-- PLUGINS SETTINGS
require'mc.plugin.octolua'.digest()
vim.cmd('colorscheme ayu') 




-- Temp
require'hardline'.setup {
}

require"toggleterm".setup {
  size = 20,
  open_mapping = [[<c-\>]],
  shade_filetypes = {},
  shade_terminals = true,
  persist_size = true,
  direction = 'horizontal',
}

-- CLOSURE
vim.o.secure = true
