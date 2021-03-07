-- DOWNLOAD PACKER {{{1
local directory = string.format(
  '%s/site/pack/packer/opt',
  vim.fn.stdpath('data')
)
local image = string.format(
  '%s/packer.nvim',
  directory
)
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

-- PACKER MAIN {{{1
local packer = nil
local function init()
  -- INIT {{{2
  if packer == nil then
    packer = require('packer')
    packer.init({
      disable_commands = true,
      -- TODO Float Window {{{3
      --[[
      display = {
        open_fn = function(name)
          local last_win = vim.api.nvim_get_current_win()
          local last_pos = vim.api.nvim_win_get_cursor(last_win)
          local columns = vim.o.columns
          local lines = vim.o.lines
          local width = math.ceil(columns * 0.8)
          local height = math.ceil(lines * 0.8 - 4)
          local left = math.ceil((columns - width) * 0.5)
          local top = math.ceil((lines - height) * 0.5 - 1)

          local opts = {
            relative = 'editor',
            style = 'minimal',
            width = width,
            height = height,
            col = left,
            row = top
          }

          local buf = vim.api.nvim_create_buf(false, false)
          local win = vim.api.nvim_open_win(buf, true, opts)

          function restore_cursor()
            vim.api.nvim_set_current_win(last_win)
            vim.api.nvim_win_set_cursor(last_win, last_pos)
          end

          vim.cmd('autocmd! BufWipeout <buffer> lua restore_cursor()')

          return win, buf
        end
      }
      --]]

      -- }}}
    })
  end

  local use = packer.use
  -- local use_rocks = packer.use_rocks
  packer.reset()
  -- }}}

  -- PACKAGES {{{2
  -- Let Packer Manage Itself
  use { 'wbthomason/packer.nvim',
    opt = true
  }
  -- UTILITIES {{{3
  use 'neovim/nvim-lspconfig'
  use 'liuchengxu/vista.vim'

  use { 'neoclide/coc.nvim',
    branch = 'release'
  }
  use { 'rafcamlet/coc-nvim-lua',
    require = { 'neoclide/coc.nvim',
      branch = 'release'}
  }
  use { 'wellle/tmux-complete.vim',
    require = { 'neoclide/coc.nvim',
      branch = 'release'}
  }

  -- GraveYard {{{
  --[=====[
  use 'nvim-lua/completion-nvim'                   -- Completion
  use { 'steelsojka/completion-buffers',
    requires = { 'nvim-lua/completion-nvim' },
  }
  use { 'albertoCaroM/completion-tmux',
    requires = { 'nvim-lua/completion-nvim' },
  }
  use { 'nvim-treesitter/completion-treesitter',
    requires = { {'nvim-lua/completion-nvim'}, {'nvim-treesitter/nvim-treesitter'} },
  }

  -- Tree Sitter: Syntax, Indentation, TextObject, Foldings.... SYNTAX AWARE.
  use { 'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
  }
  use { 'nvim-treesitter/nvim-treesitter-refactor',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  use { 'nvim-treesitter/nvim-treesitter-textobjects',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  use { 'nvim-treesitter/playground',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  --]=====]
  -- }}}

  use 'junegunn/fzf'
  use { 'junegunn/fzf.vim',
    requires = { 'junegunn/fzf' },
  }

  use { 'CantoroMC/slimux',  -- On tmux
    opt = true,
    cmd = { 'SlimuxREPLConfigure', 'SlimuxShellConfigure', 'SlimuxGlobalConfigure' },
  }
  -- }}}
  -- GUI UTILITIES {{{3
  -- Colorschemes {{{4
  use 'CantoroMC/ayu-vim'
  use 'sjl/badwolf'
  use 'morhetz/gruvbox'
  use 'NLKNguyen/papercolor-theme'
  use 'srcery-colors/srcery-vim'
  use 'eemed/sitruuna.vim'
  -- }}}
  use { 'kyazdani42/nvim-tree.lua',
    as = 'nvim-tree',
    requires = 'kyazdani42/nvim-web-devicons'
  }

  use { 'lukas-reineke/indent-blankline.nvim', -- Show indent line and leading spaces
    branch = 'lua'
  }
  use { 'norcalli/nvim-colorizer.lua',         -- Show RGB,HTML... Colors
    as = 'nvim-colorizer'
  }
  use { 'mhinz/vim-startify',                  -- Start page and session management
    requires = 'ryanoasis/vim-devicons',       -- Maybe this can be removed
  }
  use { 'vim-airline/vim-airline',             -- Status and Tab lines
    requires = 'vim-airline/vim-airline-themes'
  }
  use { 'lewis6991/gitsigns.nvim',             -- Git signs on the signcolumn
    requires = {
      'nvim-lua/plenary.nvim'
    },
    as = 'gitsigns',
  }
  use 'CantoroMC/nvim-nuake'                  -- Terminal Wrapper
  -- }}}
  -- HIS HOLINESS {{{3
  use 'tpope/vim-apathy'         -- `path`, `suffixesadd`, `include`, `includeexpr` and `define`
  use 'tpope/vim-abolish'        -- Language friendly searches, substitutions and abbreviations
  use 'tpope/vim-characterize'
  use 'tpope/vim-commentary'     -- Comment stuff out
  use { 'tpope/vim-dispatch',    -- Asynchronous build and test dispatcher
    opt = true,
    cmd = { 'Make', 'Start', 'Dispatch', 'Focus', 'FocusDispatch' },
  }
  use 'tpope/vim-endwise'        -- wisely add `end`
  use 'tpope/vim-eunuch'         -- Vim sugar for the unix shell commands that need it the most
  use 'tpope/vim-fugitive'       -- Git wrapper
  use 'tpope/vim-scriptease'     -- A Vim plugin for vim plugins
  use 'tpope/vim-speeddating'     -- A Vim plugin for vim plugins
  use 'tpope/vim-surround'       -- quotizing/parenthesizing (and more) made simple
  use 'tpope/vim-repeat'         -- enable repeating supported plugin maps with `.`
  use 'tpope/vim-rhubarb'        -- GitHub extension for fugitive.vim
  use 'CantoroMC/vim-unimpaired' -- Pairs of handy bracket mappings
  -- }}}
  -- TEXT MANIPULATION {{{3
  use { 'godlygeek/tabular',
    opt = true,
    cmd = { 'Tabularize' },
  }
  use 'christoomey/vim-sort-motion'
  use { 'mbbill/undotree',
    opt = true,
    cmd = 'UndotreeToggle'
  }
  use { 'SirVer/ultisnips',
    requires = 'honza/vim-snippets'
  }
  -- }}}
  -- FILETYPE PLUGINS {{{3
  use 'neovimhaskell/haskell-vim'       -- Haskell
  use 'euclidianace/betterlua.vim'      -- Lua
  use { 'iamcco/markdown-preview.nvim', -- Markdown
    run = ':call mkdp#util#install()',
  }
  use 'CantoroMC/vim-rasi'              -- Rofi Advanced Style Information
  use { 'norcalli/nvim-terminal.lua',
    as = 'nvim-terminal'
  }
  -- }}}
  -- VIM DEVELOPMENT {{{3
  use 'dstein64/vim-startuptime'
  -- }}}
  -- }}}
end
-- }}}

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end
})

return plugins

-- vim:fdm=marker:nospell
