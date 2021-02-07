-- Install Packer {{{1
local directory = string.format(
	'%s/site/pack/packer/opt',
	vim.fn.stdpath('data')
)
local image = string.format(
	'%s/packer.nvim',
	directory
)
print(image)
local isPacker = vim.fn.isdirectory(image)
if isPacker == 0 then
	if vim.fn.input('Download Packer? (y for yes) ') ~= 'y' then
		return
	end

	local repo = 'https://github.com/wbthomason/packer.nvim'
	local command = string.format(
		'!git clone %s %s',
		repo, image
	)
	vim.cmd(command)

	print("Downloaded packer.nvim...")
	print("You need to restart before installing plugins.")
	return
end
-- }}}

local packer = nil

local function init()
  if packer == nil then
    packer = require('packer')
    packer.init({
      disable_commands = true,
      -- TODO Float Window {{{1
      -- display = {
      --   open_fn = function(name)
      --     local last_win = vim.api.nvim_get_current_win()
      --     local last_pos = vim.api.nvim_win_get_cursor(last_win)
      --     local columns = vim.o.columns
      --     local lines = vim.o.lines
      --     local width = math.ceil(columns * 0.8)
      --     local height = math.ceil(lines * 0.8 - 4)
      --     local left = math.ceil((columns - width) * 0.5)
      --     local top = math.ceil((lines - height) * 0.5 - 1)

      --     local opts = {
      --       relative = 'editor',
      --       style = 'minimal',
      --       width = width,
      --       height = height,
      --       col = left,
      --       row = top
      --     }

      --     local buf = vim.api.nvim_create_buf(false, false)
      --     local win = vim.api.nvim_open_win(buf, true, opts)

      --     function restore_cursor()
      --       vim.api.nvim_set_current_win(last_win)
      --       vim.api.nvim_win_set_cursor(last_win, last_pos)
      --     end

      --     vim.cmd('autocmd! BufWipeout <buffer> lua restore_cursor()')

      --     return win, buf
      --   end
      -- }
      -- }}}
    })
  end

  local use = packer.use
  local use_rocks = packer.use_rocks
  packer.reset()

  -- Let Packer Manage Itself
  use { 'wbthomason/packer.nvim',
    opt = true 
  }

  -- Section: UTILITIES {{{1
  -- use { 'neoclide/coc.nvim',
  --   branch = 'release'
  -- }
  use { 'junegunn/fzf',
    requires = { 'junegunn/fzf.vim' },
  }
  use { 'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
  }

  -- }}}

  -- Section: GUIFICATION {{{1

  -- Colorschemes
  use 'CantoroMC/ayu-vim'
  use 'NLKNguyen/papercolor-theme'
  use 'morhetz/gruvbox'
  use 'srcery-colors/srcery-vim'

  use 'Yggdroot/indentLine'
  use { 'CantoroMC/nvim-tree.lua',
    as = 'nvim-tree',
    requires = { 'kyazdani42/nvim-web-devicons' },
  }

  use { 'mbbill/undotree',
    cmd = 'UndotreeToggle'
  }
  -- }}}

  -- Section: TEXT MANIPULATION {{{1
  use { 'godlygeek/tabular',
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
  use { 'iamcco/markdown-preview.nvim',
        ft  = 'markdown',
        run = ':call mkdp#util#install()',
        cmd = 'MarkdownPreview'
      }
  -- }}}
end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

-- vim:fdm=marker
