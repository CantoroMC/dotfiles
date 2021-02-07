-- GENERAL SETTINGS

-- TODO: find a better alternative
local appendWithComma = function(opt, app)
  if opt == nil or opt == "" then
    return app
  else
    return opt .. ',' .. app
  end
end

if vim.fn.has 'vim_starting' == 1 then -- Encoding
  Set_var { encoding = { 'g', 'utf-8'} }
  Set_opt {
    fileencodings = { 'o', 'ucs-bom,utf-8,default,latin1' },
    fileformats   = { 'o', 'unix,dos' },
  }
end

Set_opt { -- Text
  ambiwidth    = { 'o', 'single' },
  backspace    = { 'o', 'indent,eol,start' },
  colorcolumn  = { 'w', appendWithComma(vim.wo.colorcolumn, '80') },
  conceallevel = { 'w', 2 },
  emoji        = { 'o', true },
  fillchars    = { 'o', appendWithComma(vim.o.fillchars, 'fold:-,eob:~') },
  iskeyword    = { 'b', appendWithComma(vim.bo.iskeyword, '-') },
  list         = { 'w', true },
  listchars    = { 'o', 'tab:>-,trail:·,nbsp:+,eol:¬,precedes:←,extends:→' },
  matchpairs   = { 'b', appendWithComma(vim.bo.matchpairs, '<:>') },
  matchtime    = { 'o', 5 },
  nrformats    = { 'b', 'bin,hex' },
  scrolloff    = { 'o', 3 },
  showmatch    = { 'o', true },
  startofline  = { 'o', true },
  synmaxcol    = { 'b', 3000 },
  textwidth    = { 'b', 80 },
  virtualedit  = { 'o', "" },
}

Set_opt { -- Wrapping
  breakindent    = { 'w', true },
  breakindentopt = { 'w', 'min:10,shift:4,sbr' },
  linebreak      = { 'w', true },
  sidescroll     = { 'o', 5 },
  sidescrolloff  = { 'o', 2 },
  showbreak      = { 'o', [[  ↪\]] },
  whichwrap      = { 'o', 'b,s' },
  wrap           = { 'w', true },
}

Set_opt { -- Generic
  belloff       = { 'o', 'all' },
  errorbells    = { 'o', false },
  visualbell    = { 'o', false },
  clipboard     = { 'o', appendWithComma(vim.o.clipboard, 'unnamed') },
  confirm       = { 'o', true },
  cpoptions     = { 'o', vim.o.cpoptions .. '$' },
  diffopt       = { 'o', appendWithComma(vim.o.diffopt, 'vertical,context:2,iwhite') },
  modelines     = { 'o', 1 },
  mouse         = { 'o', 'a' },
  lazyredraw    = { 'o', false },
  termguicolors = { 'o', true },
  timeout       = { 'o', true },
  timeoutlen    = { 'o', 1000 },
  ttimeout      = { 'o', true },
  ttimeoutlen   = { 'o', 50 },
}

Set_opt { -- Sign Column
  number         = { 'w', true },
  numberwidth    = { 'w', 1 },
  relativenumber = { 'w', true },
  signcolumn     = { 'w', 'auto' },
}

Set_opt { -- Command Line
  cedit          = { 'o', '<Home>' },
  cmdheight      = { 'o', 2 },
  inccommand     = { 'o', 'split' },
  shortmess      = { 'o', 'atTcoOF' },
  showcmd        = { 'o', true },
  wildchar       = { 'o', 9 },   -- corresponding to <Tab>
  wildcharm      = { 'o', 9 },
  wildignore     = { 'o', appendWithComma(vim.o.wildignore,
                          '*~,*.o,*.obj,*.aux,*.git,*.svn,*.pyc,__pycache__') },
  wildignorecase = { 'o', true },
  wildmenu       = { 'o', true },
  wildmode       = { 'o', 'longest:full,full' },
  wildoptions    = { 'o', 'pum' },
}

Set_opt { -- Status And Tab Lines
  laststatus  = { 'o', 2 },
  ruler       = { 'o', true },
  showmode    = { 'o', true },
  showtabline = { 'o', 1 },
}

Set_opt { -- Windows
  equalalways = { 'o', true },
  splitbelow  = { 'o', true },
  splitright  = { 'o', true },
}

Set_opt { -- Indentation
  shiftround  = { 'o', true },
  autoindent  = { 'b', true },
  smartindent = { 'b', true },
}


vim.cmd('augroup filetype_indentation')
  -- TODO: add preserveindent and copyindent for tabs
  vim.cmd('autocmd!')
    vim.cmd(FtAuCmd( -- Only Spaces Four Characters
      { 'python', 'matlab', 'help', 'lisp', 'haskell', 'lhaskell' },
      { 'tabstop=4', 'softtabstop=4', 'expandtab', 'shiftwidth=4' }))
    vim.cmd(FtAuCmd( -- Only Spaces Two Characters
      { 'vim', 'sh', 'zsh', 'markdown', 'tex', 'plaintex', 'bib', 'text', 'yaml',
        'ruby', 'xml', 'xdefaults', 'tmux', 'cabal', 'nix', 'lua' },
      { 'tabstop=2', 'softtabstop=2', 'expandtab', 'shiftwidth=2' }))
    vim.cmd(FtAuCmd( -- Tabs With Two Characters:
      { 'make', 'json', 'html', 'css', 'javascript', 'neomuttrc', 'muttrc' },
      { 'tabstop=2', 'softtabstop=2', 'noexpandtab', 'shiftwidth=2' }))
    vim.cmd(FtAuCmd( -- Tabs With Four Characters:
      { 'go', 'c', 'cpp', 'h' },
      { 'tabstop=4', 'softtabstop=4', 'noexpandtab', 'shiftwidth=4' }))
    -- C Indentation:
    vim.cmd(FtAuCmd(
      { 'c', 'cpp', 'h' },
      { 'cindent' }))
vim.cmd('augroup END')

Set_opt { -- Folding
  foldenable     = { 'w', true },
  foldcolumn     = { 'w', '1' },
  foldlevelstart = { 'o', 0 },
  foldmarker     = { 'w', '{{{,}}}' },
  foldnestmax    = { 'w', 10 },
}

vim.cmd('augroup fyletype_folding_method')
  vim.cmd('autocmd!')
  vim.cmd(FtAuCmd( -- Markers
    { 'vim', 'tex', 'plaintex', 'text', 'neomuttrc', 'muttrc', 'tmux' },
    { 'foldmethod=marker' }))
  vim.cmd(FtAuCmd( -- Indent
    { 'make', 'python', 'bib', 'go', 'json', 'html', 'css', 'javascript', 'yaml',
     'ruby', 'xml', 'haskell', 'lhaskell', 'cabal', 'nix', 'lua', 'lisp' },
    { 'foldmethod=indent' }))
  vim.cmd(FtAuCmd( -- Syntax
    { 'c', 'cpp', 'git', 'gitcommit', 'zsh', 'sh' },
    { 'foldmethod=syntax' }))
vim.cmd('augroup END')

Set_opt { -- Files And Buffers
  autoread  = { 'o', true },
  autowrite = { 'o', true },
  hidden    = { 'o', true },
  path      = { 'o', appendWithComma(vim.o.path, '**') },
  switchbuf = { 'o', 'useopen,usetab' },
}

Set_opt { -- Spelling
  langremap = { 'o', false },
  spelllang = { 'b', 'en,it' },
  spellfile = { 'b', os.getenv('XDG_CONFIG_HOME')..'/nvim/spell/dictionary.utf-8.add' },
  spell     = { 'w', true },
}

Set_opt { -- Insert Mode Completion
  complete    = { 'b', appendWithComma(vim.bo.complete, 'i,kspell') },
  completeopt = { 'o', 'menuone,preview,noinsert,noselect' },
  dictionary  = { 'o', appendWithComma(vim.o.dictionary, 'spell') },
  omnifunc    = { 'b', 'syntaxcomplete#Complete' },
  showfulltag = { 'o', true },
}

Set_opt { -- Pop Up Menu
  pumblend  = { 'o', 30 },
  pumheight = { 'o', 20 },
}

Set_opt { -- Safety
  backup      = { 'o', false },
  swapfile    = { 'b', true },
  updatetime  = { 'o', 1000 },
  writebackup = { 'o', true },
}

Set_opt { -- History
  undodir        = { 'o', os.getenv('XDG_DATA_HOME')..'/nvim/undo' },
  undofile       = { 'b', true },
  history        = { 'o', 200 },
  sessionoptions = { 'o', 'blank,buffers,curdir,folds,tabpages,winsize' },
  viewdir        = { 'o', os.getenv('XDG_DATA_HOME')..'/nvim/view' },
  viewoptions    = { 'o', 'cursor,curdir,folds,options' },
}

local function chooseGrepper()
  if os.execute('whereis rg &>/dev/null') then
    return [[rg -s --hidden --vimgrep]]
  elseif os.execute('whereis ag &>/dev/null') then
    return [[ag -sR --hidden --vimgrep]]
  else
    return [[grep -rRHnIi $* /dev/null]]
  end
end

Set_opt { -- Searching
  grepformat   = { 'o', [[%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f %l%m]] },
  grepprg      = { 'o', chooseGrepper() },
  hlsearch     = { 'o', true },
  ignorecase   = { 'o', true },
  incsearch    = { 'o', true },
  regexpengine = { 'o', 0 },
  smartcase    = { 'o', true },
  tagcase      = { 'o', 'followscs' },
}


-- DISTRIBUTION PLUGINS AND SCRIPTS OPTIONS

Set_var { -- Disable Unwanted Plugins
  loaded_2html_plugin      = { 'g', 1 },
  loaded_gzip              = { 'g', 1 },
  loaded_tar               = { 'g', 1 },
  loaded_tarPlugin         = { 'g', 1 },
  loaded_zip               = { 'g', 1 },
  loaded_zipPlugin         = { 'g', 1 },
  loaded_tutor_mode_plugin = { 'g', 1 },
}

-- Add Optional Packages
vim.cmd 'packadd! justify'    -- Text Justification
vim.cmd 'packadd! cfilter'    -- Quickfix list filtering
-- vim.cmd 'packadd! termdebug'  -- Terminal debugger
-- vim.cmd 'packadd! vimball'    -- Make life easier with plugins

Set_var { -- Scripts Variables
  -- Matchit
  match_ignorecase = { 'b', 1 },

  -- FILETYPES
  -- Lisp
  lisp_rainbow     = { 'g', 1 }, -- parenthesis highlighting for lisp
  lisp_instring    = { 'g', 1 }, -- strings (...) highlighted as string lisp
  lispsyntax_clisp = { 'g', 1 }, -- clisp syntax

  -- Markdown
  markdown_fenced_languages = { 'g', -- Highlighting of chunks of language code
    { 'bash=sh',
      'c',
      'cpp',
      'help',
      'html',
      'matlab',
      'python',
      'tex',
      'vim',
    }
  },

  -- Python
  python_highlight_all = { 'g', 1 }, -- Allow all possible highlightings ( defined as local in theory )

  -- Tex
  --[[ Conceal Options
    | a |   accents/ligatures        |
    | b |   bold and italic          |
    | d |   delimiters               |
    | m |   math symbols             |
    | g |   Greek                    |
    | s |   superscripts/subscripts  |
  --]]
  tex_conceal = { 'g', 'abdmgs' }, -- Conceal Options
  tex_flavor  = { 'g', 'latex' },  -- Default TeX Flavor

  -- Vim
  vimsyn_embed   = { 'g', 'lPr' }, -- Embedded script highlighting supported
                                   -- (Lua,Py,Ruby) or 0 for disabling
  vimsyn_folding = { 'g', 'afP' }, -- folding for syntax (augroup, fold fun, Python)

  -- Zsh
  sh_fold_enabled = { 'g', 7 }, -- fold for function,heredoc and if/do/for with fdm=syntax
  zsh_fold_enable = { 'g', 1 }, -- allow for syntax based foldings
}
