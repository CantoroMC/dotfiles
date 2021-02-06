-- Automatically Install Packer if required {{{1
local packerDir = Fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if Fn.isdirectory(packerDir) ~= 1 then
  Cmd('!git clone https://github.com/wbthomason/packer.nvim ' .. packerDir )
end
-- }}}


Cmd('packadd packer.nvim')
return require('packer').startup(
  function(use)
    use { 'wbthomason/packer.nvim', opt = true }

    -- Section: UTILITIES {{{1
    use {
      'neoclide/coc.nvim',
      branch = 'release'
    }
    use {
      'junegunn/fzf',
      requires = { 'junegunn/fzf.vim' },
    }
    -- }}}

    -- Section: GUIFICATION {{{1

    -- Colorschemes
    use 'CantoroMC/ayu-vim'
    use 'NLKNguyen/papercolor-theme'

    use 'Yggdroot/indentLine'
    use {
      'CantoroMC/nvim-tree.lua',
      requires = { 'kyazdani42/nvim-web-devicons' },
    }

    -- }}}

    -- Section: TEXT MANIPULATION {{{1
    use {
      'godlygeek/tabular',
      opt = true,
      cmd = { 'Tabularize' },
    }
    -- }}}

    -- Section: HIS HOLINESS {{{1
    use 'tpope/vim-abolish'
    use 'tpope/vim-commentary'
    use 'tpope/vim-endwise'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-surround'
    use 'tpope/vim-repeat'
    use 'tpope/vim-rhubarb'
    use 'CantoroMC/vim-unimpaired'
    use {
      'tpope/vim-dispatch',
      opt = true,
      cmd = { 'Make', 'Start', 'Dispatch', 'Focus', 'FocusDispatch' },
    }
    -- }}}

    -- Section: FILETYPE PLUGINS {{{1

    -- }}}

  end
)

-- vim:fdm=marker
