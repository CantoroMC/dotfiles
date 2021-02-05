-- Automatically Install Packer if required {{{1
local packerDir = vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if vim.fn.isdirectory(packerDir) ~= 1 then
  vim.cmd('!git clone https://github.com/wbthomason/packer.nvim ' .. packerDir )
end
-- }}}

vim.cmd('packadd packer.nvim')
return require('packer').startup(
  function()
    use { 'wbthomason/packer.nvim', opt = true }

    -- Utilities: {{{2
    use {
      'neoclide/coc.nvim',
      branch = 'release'
    }

    -- }}}

    -- Section: Gui {{{1
    use 'CantoroMC/ayu-vim'
    -- }}}

    -- Section: His Holiness {{{1
    use 'tpope/vim-abolish'
    use 'tpope/vim-commentary'
    use 'tpope/vim-endwise'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-surround'
    use 'tpope/vim-repeat'
    use 'tpope/vim-rhubarb'
    use { 
      'tpope/vim-dispatch',
      opt = true, 
      cmd = { 'Make', 'Start', 'Dispatch', 'Focus', 'FocusDispatch' }
    }
    -- }}}

  end 
)

