-- Install Packer {{{1
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

local packer = nil

local function init()

  if packer == nil then
    packer = require('packer')
    packer.init({
      disable_commands = true
      -- TODO Float Window {{{1
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

  -- Let Packer Manage Itself
  use { 'wbthomason/packer.nvim',
    opt = true
  }

  -- Section: UTILITIES {{{1

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

  use 'neovim/nvim-lspconfig'                      -- Language Server Protocol

  -- GraveYard {{{

  -- use 'nvim-lua/completion-nvim'                   -- Completion
  -- use { 'steelsojka/completion-buffers',
  --   requires = { 'nvim-lua/completion-nvim' },
  -- }
  -- use { 'albertoCaroM/completion-tmux',
  --   requires = { 'nvim-lua/completion-nvim' },
  -- }
  -- use { 'nvim-treesitter/completion-treesitter',
  --   requires = { {'nvim-lua/completion-nvim'}, {'nvim-treesitter/nvim-treesitter'} },
  -- }
  -- -- Tree Sitter: Syntax, Indentation, TextObject, Foldings.... SYNTAX AWARE.
  -- use { 'nvim-treesitter/nvim-treesitter',
  --   run = ':TSUpdate',
  -- }
  -- use { 'nvim-treesitter/nvim-treesitter-refactor',
  --   requires = 'nvim-treesitter/nvim-treesitter',
  -- }
  -- use { 'nvim-treesitter/nvim-treesitter-textobjects',
  --   requires = 'nvim-treesitter/nvim-treesitter',
  -- }
  -- use { 'nvim-treesitter/playground',
  --   requires = 'nvim-treesitter/nvim-treesitter',
  -- }
  -- }}}

  use 'junegunn/fzf'                               -- Commands wrapped around fzf
  use { 'junegunn/fzf.vim',
    requires = { 'junegunn/fzf' },
  }

  --[=[ This seems cool and not so intrusive
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      {'nvim-lua/popup.nvim'},
      {'nvim-lua/plenary.nvim'}
    }
  }
  --]=]

  -- }}}


  -- Section: GUIFICATION {{{1

  use 'CantoroMC/ayu-vim'               -- Colorschemes
  use 'sjl/badwolf'
  use 'morhetz/gruvbox'
  use 'NLKNguyen/papercolor-theme'
  use 'srcery-colors/srcery-vim'

  use { 'CantoroMC/nvim-tree.lua',      -- File Explorer
    as = 'nvim-tree',
    requires = { 'kyazdani42/nvim-web-devicons' },
  }

  use 'Yggdroot/indentLine'             -- Show indent line and leading spaces
  use { 'norcalli/nvim-colorizer.lua',  -- Show RGB,HTML... Colors
    as = 'nvim-colorizer'
  }

  use { 'mbbill/undotree',              -- Vim undo shown ad a Tree
    cmd = 'UndotreeToggle'
  }

 use { 'mhinz/vim-startify',             -- Start page and session management
    requires = 'ryanoasis/vim-devicons', -- Maybe this can be removed
  }

  -- }}}


  -- Section: HIS HOLINESS {{{1

  use 'tpope/vim-apathy'         -- `path`, `suffixesadd`, `include`, `includeexpr` and `define`
  use 'tpope/vim-abolish'        -- Language friendly searches, substitutions and abbreviations
  use 'tpope/vim-commentary'     -- Comment stuff out
  use { 'tpope/vim-dispatch',    -- Asynchronous build and test dispatcher
    opt = true,
    cmd = { 'Make', 'Start', 'Dispatch', 'Focus', 'FocusDispatch' },
  }
  use 'tpope/vim-endwise'        -- wisely add `end`
  use 'tpope/vim-eunuch'         -- Vim sugar for the unix shell commands that need it the most
  use 'tpope/vim-fugitive'       -- Git wrapper
  use 'tpope/vim-scriptease'     -- A Vim plugin for vim plugins
  use 'tpope/vim-surround'       -- quotizing/parenthesizing (and more) made simple
  use 'tpope/vim-repeat'         -- enable repeating supported plugin maps with `.`
  use 'tpope/vim-rhubarb'        -- GitHub extension for fugitive.vim
  use 'CantoroMC/vim-unimpaired' -- Pairs of handy bracket mappings

  -- }}}


  -- Section: TEXT MANIPULATION {{{1

  use { 'godlygeek/tabular',
    opt = true,
    cmd = { 'Tabularize' },
  }

  -- }}}


  -- Section: FILETYPE PLUGINS {{{1

  use 'neovimhaskell/haskell-vim'       -- Haskell

  use 'euclidianace/betterlua.vim'      -- Lua

  use { 'iamcco/markdown-preview.nvim', -- Markdown
    run = ':call mkdp#util#install()',
  }


  use 'CantoroMC/vim-rasi'              -- Rofi Advanced Style Information

  -- }}}


-- VIM DEVELOPMENT {{{1

use 'dstein64/vim-startuptime'

-- }}}


-- TODO: {{{1

  use '~/Desktop/NeoLuaGit/nvim-hardline'
  use '~/Desktop/PluginsBullici/nuake'
  use '~/Desktop/NeoLuaGit/nvim-toggleterm.lua'

  -- REPLs
  use { 'hkupty/iron.nvim',  -- On nvim terminal in lua
    as = 'iron'
  }
  use { 'CantoroMC/slimux',  -- On tmux
    cmd = { 'SlimuxREPLConfigure', 'SlimuxShellConfigure', 'SlimuxGlobalConfigure' },
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
