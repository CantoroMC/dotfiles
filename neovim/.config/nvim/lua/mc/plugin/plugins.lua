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
      -- Float Window {{{3
      display = {
        open_fn = function(name)
          -- main window
          local last_win = vim.api.nvim_get_current_win()
          local last_pos = vim.api.nvim_win_get_cursor(last_win)

          local columns = vim.o.columns
          local lines   = vim.o.lines

          local width  = math.ceil(columns * 0.8)
          local height = math.ceil(lines * 0.8 - 4)
          local left   = math.ceil((columns - width) * 0.5)
          local top    = math.ceil((lines - height) * 0.5 - 1)

          local opts = {
            relative = 'editor',
            style    = 'minimal',
            width    = width,
            height   = height,
            col      = left,
            row      = top,
          }

          local buf = vim.api.nvim_create_buf(false, false)


          -- border window
          local border_buf = vim.api.nvim_create_buf(false, true)
          local border_opts = {
            relative = 'editor',
            style    = 'minimal',
            width    = width + 2,
            height   = height + 2,
            col      = left - 1,
            row      = top - 1
          }
          local border_lines = { }
          local top_line    = '╔' .. string.rep('═', width) .. '╗'
          local middle_line = '║' .. string.rep(' ', width) .. '║'
          local bottom_line = '╚' .. string.rep('═', width) .. '╝'
          table.insert(border_lines, top_line)
          for i = 1, height do
            table.insert(border_lines, middle_line)
          end
          table.insert(border_lines, bottom_line)

          vim.api.nvim_buf_set_lines(border_buf, 0, -1, false, border_lines)
          local border_win = vim.api.nvim_open_win(border_buf, true, border_opts)
          vim.api.nvim_win_set_option(border_win, 'winblend', 10)
          vim.api.nvim_win_set_option(border_win, 'winhighlight', 'Normal:Normal')


          local win = vim.api.nvim_open_win(buf, true, opts)
          vim.api.nvim_buf_set_name(buf, name)
          vim.api.nvim_win_set_option(win, 'winblend', 10)
          vim.api.nvim_win_set_option(win, 'winhighlight', 'Normal:Normal')

          -- post tasks
          function restore_cursor()
            vim.api.nvim_set_current_win(last_win)
            vim.api.nvim_win_set_cursor(last_win, last_pos)
          end

          vim.cmd('autocmd! BufWipeout <buffer> lua restore_cursor()')
          vim.cmd('autocmd! BufWipeout <buffer> execute "silent bwipeout! "'..border_buf)

          return true, win, buf
        end
      -- }}}
      }
    })
  end
  -- }}}

  local use = packer.use
  -- local use_rocks = packer.use_rocks
  packer.reset()

  -- PACKAGES {{{2
  -- Let Packer Manage Itself
  use { 'wbthomason/packer.nvim',
    opt = true
  }
  -- UTILITIES {{{3

  -- NeoVim Language Server Protocol And Completion {{{4
  use 'neovim/nvim-lspconfig'
  use 'kosayoda/nvim-lightbulb'
  use 'liuchengxu/vista.vim'

  --[==[
  use 'hrsh7th/nvim-compe'
  use {
    'onsails/lspkind-nvim',
    requires = { 'hrsh7th/nvim-compe' }
  }
  use {
    'tamago324/compe-zsh',
    requires = {
      'hrsh7th/nvim-compe',
      'nvim-lua/plenary.nvim'
    }
  }
  use {
    'GoldsteinE/compe-latex-symbols',
    requires = { 'hrsh7th/nvim-compe' },
  }
  --]==]
  -- }}}

  -- Tree Sitter: Syntax, Indentation, TextObject, Foldings.... SYNTAX AWARE. {{{4
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
  }
  use {
    'nvim-treesitter/nvim-treesitter-refactor',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  use {
    'nvim-treesitter/nvim-treesitter-textobjects',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  use {
    'nvim-treesitter/playground',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  use {
    'p00f/nvim-ts-rainbow',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  -- }}}

  -- Conquer Of Completion {{{4
  use {
    'neoclide/coc.nvim',
    branch = 'release'
  }
  use {
    'rafcamlet/coc-nvim-lua',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  use { 'wellle/tmux-complete.vim',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  use { 'CantoroMC/coc-latex_symbols',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  -- }}}

  -- Tag Viewer with Ctags
  use 'preservim/tagbar'
  -- FuzzyFinder
  use 'junegunn/fzf'
  use { 'junegunn/fzf.vim',
    requires = { 'junegunn/fzf' },
  }
  -- Snippets
  use {
    'SirVer/ultisnips',
    requires = 'honza/vim-snippets'
  }
  -- REPL around Tmux
  use {
    'CantoroMC/slimux',
    opt = true,
    cmd = {
      'SlimuxREPLConfigure',
      'SlimuxShellConfigure',
      'SlimuxGlobalConfigure'
    },
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

  -- File Explorer
  use {
    'kyazdani42/nvim-tree.lua',
    as = 'nvim-tree',
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- Indent line and Leading spaces
  use {
    'lukas-reineke/indent-blankline.nvim',
    branch = 'lua'
  }
  -- Show RGB,HTML... Colors
  use {
    'norcalli/nvim-colorizer.lua',
    as = 'nvim-colorizer'
  }
  -- Start page and session management
  use {
    'mhinz/vim-startify',
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- Status and Tab lines
  use {
    'vim-airline/vim-airline',
    requires = {
      'vim-airline/vim-airline-themes',
      'kyazdani42/nvim-web-devicons',
    }
  }
  -- Git signs on the signcolumn
  use { 'lewis6991/gitsigns.nvim',
    commit = '5be4faa',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    as = 'gitsigns',
  }
  -- Terminal Wrapper
  use 'CantoroMC/nvim-nuake'
  -- Linux Info
  use {
    'HiPhish/info.vim',
    opt = true,
    cmd = 'Info',
  }
  -- }}}

  -- HIS HOLINESS {{{3
  use 'tpope/vim-apathy'         -- `path`, `suffixesadd`, `include`, `includeexpr` and `define`
  use 'tpope/vim-abolish'        -- Language friendly searches, substitutions and abbreviations
  use 'tpope/vim-characterize'   -- Unicode character metadata (with `ga`)
  use 'tpope/vim-commentary'     -- Comment stuff out
  use {
    'tpope/vim-dispatch',        -- Asynchronous build and test dispatcher
    opt = true,
    cmd = {
      'Make',
      'Start',
      'Dispatch',
      'Focus',
      'FocusDispatch'
    },
  }
  use 'tpope/vim-endwise'        -- wisely add `end`
  use 'tpope/vim-eunuch'         -- Vim sugar for the unix shell commands that need it the most
  use 'tpope/vim-fugitive'       -- Git wrapper
  use 'tpope/vim-scriptease'     -- A Vim plugin for vim plugins
  use 'tpope/vim-speeddating'    -- <CTRL-A>/<CTRL-X> to increment dates, times and dates
  use 'tpope/vim-surround'       -- quotizing/parenthesizing (and more) made simple
  use 'tpope/vim-repeat'         -- enable repeating supported plugin maps with `.`
  use 'tpope/vim-rhubarb'        -- GitHub extension for fugitive.vim
  use 'CantoroMC/vim-unimpaired' -- Pairs of handy bracket mappings
  -- }}}

  -- TEXT MANIPULATION {{{3
  use {
    'godlygeek/tabular',
    opt = true,
    cmd = 'Tabularize',
  }
  use 'christoomey/vim-sort-motion'
  use {
    'mbbill/undotree',
    opt = true,
    cmd = 'UndotreeToggle'
  }
  -- }}}

  -- FILETYPE PLUGINS {{{3
  use {
    'neovimhaskell/haskell-vim',       -- Haskell
    ft = { 'haskell', 'lhaskell' }
  }
  use {
    'iamcco/markdown-preview.nvim',    -- Markdown
    run = ':call mkdp#util#install()',
    ft = 'markdown'
  }
  use {
    'CantoroMC/vim-rasi',              -- Rofi Advanced Style Information
    ft = 'rasi'
  }
  use {
    'norcalli/nvim-terminal.lua',
    as = 'nvim-terminal'
  }
  use {
    'KeitaNakamura/tex-conceal.vim',
    ft = { 'tex', 'context', 'plaintex' }
  }
  -- }}}

  -- VIM DEVELOPMENT {{{3
  use {
    'dstein64/vim-startuptime',
    opt = true,
    cmd = 'StartupTime',
  }
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
