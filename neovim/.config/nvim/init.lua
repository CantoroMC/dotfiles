-- NeoLua Init File

-- GLOBAL LUA VARIABLES
PP = function(v)
  print(vim.inspect(v))
  return v
end

-- SETTINGS AND MAPPINGS
vim.cmd('source ' .. vim.fn.stdpath('config') .. '/settings.vim')
vim.cmd('source ' .. vim.fn.stdpath('config') .. '/keymap.vim')

-- PLUGINS SETTINGS
require'mc.plugin.octolua'.digest()

-- COLORSCHEME: managed by a small lua script
require'mc.plugin.colorpicker'.setup {
  active = true,
  light_time = {6,8},
  themes     = {
    [ "dark" ]  = {
      [ "ayu" ]        = 'ayu_dark',
      [ "badwolf" ]    = 'badwolf',
      [ "gruvbox" ]    = 'base16_gruvbox_dark_hard',
      [ "jellybeans" ] = 'wombat',
      [ "one-nvim" ]   = 'onedark',
      [ "PaperColor" ] = 'base16_vim',
      [ "srcery" ]     = 'zenburn',
      [ "sitruuna" ]   = 'base16_pop',
    },
    [ "light" ] = {
      [ "mayansmoke" ] = 'xtermlight',
      [ "PaperColor" ] = 'papercolor',
    }
  },
  theme = { [ "sitruuna" ] = 'base16_pop' },
}

-- CLOSURE
vim.o.secure = true
