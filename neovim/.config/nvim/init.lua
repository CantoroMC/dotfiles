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

-- ColorScheme, Buffer and Tab Lines
require'hardline'.setup {
}

vim.g.nvim_tree_disable_netrw = 0

require'mc.plugin.colorpicker'.setup {
  active = true,
  light_time = {7,8},
  themes = {
    [ "dark" ]  = { 'ayu', 'badwolf', 'gruvbox', 'jellybeans', 'PaperColor', 'srcery' },
    [ "light" ] = { 'mayansmoke', 'PaperColor' }
  }
}

-- CLOSURE
vim.o.secure = true
