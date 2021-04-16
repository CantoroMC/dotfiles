-- NeoLua Init File

require('mc.util.globals')

-- SETTINGS AND MAPPINGS
vim.cmd(string.format('source %s/settings.vim', vim.fn.stdpath('config')))
vim.cmd(string.format('source %s/keymap.vim',   vim.fn.stdpath('config')))

-- PLUGINS SETTINGS
require'mc.plugin.octolua'.digest()

-- COLORSCHEME: managed by a small lua script
require'mc.plugin.colorpicker'.setup {
  active = false,
  theme = { [ "ayu" ] = 'ayu_dark'},
  themes     = {
    [ "dark" ]  = {
      [ "ayu" ]        = 'ayu_dark',
      [ "gruvbox" ]    = 'base16_gruvbox_dark_hard',
      [ "PaperColor" ] = 'base16_vim',
      [ "sitruuna" ]   = 'base16_pop',
      -- [ "jellybeans" ] = 'wombat',
      -- [ "badwolf" ]    = 'badwolf',
      -- [ "srcery" ]     = 'zenburn',
    },
    [ "light" ] = {
      [ "mayansmoke" ] = 'xtermlight',
      [ "PaperColor" ] = 'papercolor',
    }
  },
}

-- CLOSURE
vim.o.secure = true
