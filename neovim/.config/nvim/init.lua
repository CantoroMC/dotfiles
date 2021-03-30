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
