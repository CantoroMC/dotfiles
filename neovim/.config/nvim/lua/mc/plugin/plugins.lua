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
            border   = {"╔", "═" ,"╗", "║", "╝", "═", "╚", "║"},
            width    = width,
            height   = height,
            col      = left,
            row      = top,
          }

          local buf = vim.api.nvim_create_buf(false, false)

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

          return true, win, buf
        end
      -- }}}
      }
    })
  end
  -- }}}

  local usepackage = packer.use
  -- local use_rocks = packer.use_rocks
  packer.reset()

  -- PACKAGES {{{2
  -- Let Packer Manage Itself
  usepackage { 'wbthomason/packer.nvim',
    opt = true
  }
  -- UTILITIES {{{3

  -- NeoVim Language Server Protocol And Completion {{{4
  usepackage 'neovim/nvim-lspconfig'
  usepackage 'kosayoda/nvim-lightbulb'
  usepackage {
    'liuchengxu/vista.vim',
    as = 'vista',
  }

  --[==[
  usepackage 'hrsh7th/nvim-compe'
  usepackage {
    'onsails/lspkind-nvim',
    requires = { 'hrsh7th/nvim-compe' }
  }
  usepackage {
    'tamago324/compe-zsh',
    requires = {
      'hrsh7th/nvim-compe',
      'nvim-lua/plenary.nvim'
    }
  }
  usepackage {
    'GoldsteinE/compe-latex-symbols',
    requires = { 'hrsh7th/nvim-compe' },
  }
  --]==]
  -- }}}

  -- Tree Sitter: Syntax, Indentation, TextObject, Foldings.... SYNTAX AWARE. {{{4
  usepackage {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
  }
  usepackage {
    'nvim-treesitter/nvim-treesitter-refactor',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  usepackage {
    'nvim-treesitter/nvim-treesitter-textobjects',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  usepackage {
    'nvim-treesitter/playground',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  usepackage {
    'p00f/nvim-ts-rainbow',
    requires = 'nvim-treesitter/nvim-treesitter',
  }
  -- }}}

  -- Conquer Of Completion {{{4
  usepackage {
    'neoclide/coc.nvim',
    branch = 'release'
  }
  usepackage {
    'rafcamlet/coc-nvim-lua',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  usepackage { 'wellle/tmux-complete.vim',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  usepackage { 'CantoroMC/coc-latex_symbols',
    require = {
      'neoclide/coc.nvim', branch = 'release'
    }
  }
  -- }}}

  -- Tag Viewer with Ctags
  usepackage 'preservim/tagbar'
  -- FuzzyFinder
  usepackage 'junegunn/fzf'
  usepackage {
    'junegunn/fzf.vim',
    requires = { 'junegunn/fzf' },
  }
  -- Snippets
  usepackage {
    'SirVer/ultisnips',
    requires = 'honza/vim-snippets'
  }
  -- REPL around Tmux
  usepackage {
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
  usepackage 'CantoroMC/ayu-vim'
  usepackage 'sjl/badwolf'
  usepackage 'morhetz/gruvbox'
  usepackage 'NLKNguyen/papercolor-theme'
  usepackage 'srcery-colors/srcery-vim'
  usepackage 'eemed/sitruuna.vim'
  -- }}}

  -- File Explorer
  usepackage {
    'kyazdani42/nvim-tree.lua',
    as = 'nvim-tree',
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- Indent line and Leading spaces
  usepackage {
    'lukas-reineke/indent-blankline.nvim',
    branch = 'lua',
    as = 'indent-blankline'
  }
  -- Show RGB,HTML... Colors
  usepackage {
    'norcalli/nvim-colorizer.lua',
    as = 'nvim-colorizer'
  }
  -- Start page and session management
  usepackage {
    'mhinz/vim-startify',
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- Status and Tab lines
  usepackage {
    'hoob3rt/lualine.nvim',
    as       = 'lualine',
    requires = { 'kyazdani42/nvim-web-devicons' },
  }
  usepackage {
    'akinsho/nvim-bufferline.lua',
    as = 'nvim-bufferline',
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- Git signs on the signcolumn
  usepackage { 'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    as = 'gitsigns',
  }
  -- Terminal Wrapper
  usepackage 'CantoroMC/nvim-nuake'
  -- Linux Info
  usepackage {
    'HiPhish/info.vim',
    opt = true,
    cmd = 'Info',
  }
  -- }}}

  -- HIS HOLINESS {{{3
  usepackage 'tpope/vim-apathy'         -- `path`, `suffixesadd`, `include`, `includeexpr` and `define`
  usepackage 'tpope/vim-abolish'        -- Language friendly searches, substitutions and abbreviations
  usepackage 'tpope/vim-characterize'   -- Unicode character metadata (with `ga`)
  usepackage 'tpope/vim-commentary'     -- Comment stuff out
  usepackage {
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
  usepackage 'tpope/vim-endwise'        -- wisely add `end`
  usepackage 'tpope/vim-eunuch'         -- Vim sugar for the unix shell commands that need it the most
  usepackage 'tpope/vim-fugitive'       -- Git wrapper
  usepackage 'tpope/vim-scriptease'     -- A Vim plugin for vim plugins
  usepackage 'tpope/vim-speeddating'    -- <CTRL-A>/<CTRL-X> to increment dates, times and dates
  usepackage 'tpope/vim-surround'       -- quotizing/parenthesizing (and more) made simple
  usepackage 'tpope/vim-repeat'         -- enable repeating supported plugin maps with `.`
  usepackage 'tpope/vim-rhubarb'        -- GitHub extension for fugitive.vim
  usepackage 'CantoroMC/vim-unimpaired' -- Pairs of handy bracket mappings
  -- }}}

  -- TEXT MANIPULATION {{{3
  usepackage {
    'godlygeek/tabular',
    opt = true,
    cmd = 'Tabularize',
  }
  usepackage 'christoomey/vim-sort-motion'
  usepackage {
    'mbbill/undotree',
    opt = true,
    cmd = 'UndotreeToggle'
  }
  -- }}}

  -- FILETYPE PLUGINS {{{3
  usepackage {
    'neovimhaskell/haskell-vim',       -- Haskell
    ft = { 'haskell', 'lhaskell' }
  }
  usepackage {
    'iamcco/markdown-preview.nvim',    -- Markdown
    run = ':call mkdp#util#install()',
    ft = 'markdown'
  }
  usepackage {
    'CantoroMC/vim-rasi',              -- Rofi Advanced Style Information
    ft = 'rasi'
  }
  usepackage {
    'norcalli/nvim-terminal.lua',
    as = 'nvim-terminal'
  }
  usepackage {
    'KeitaNakamura/tex-conceal.vim',
    ft = { 'tex', 'context', 'plaintex' }
  }
  -- }}}

  -- VIM DEVELOPMENT {{{3
  usepackage {
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
