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

-- COLORSCHEME: managed by a small lua script
require'mc.plugin.colorpicker'.setup {
  active = true,
  light_time = {7,8},
  themes     = {
    [ "dark" ]  = {
      [ "ayu" ]        = 'ayu_dark',
      [ "badwolf" ]    = 'badwolf',
      [ "gruvbox" ]    = 'base16_gruvbox_dark_hard',
      [ "jellybeans" ] = 'wombat',
      [ "PaperColor" ] = 'base16_vim',
      [ "srcery" ]     = 'zenburn',
    },
    [ "light" ] = {
      [ "mayansmoke" ] = 'xtermlight',
      [ "PaperColor" ] = 'papercolor',
    }
  },
  theme = { [ "ayu" ] = 'ayu_dark' },
}

-- CLOSURE
vim.o.secure = true
