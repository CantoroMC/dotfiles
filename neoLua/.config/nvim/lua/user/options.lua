-- General Settings

-- TODO: find a better alternative
local appendWithComma = function(opt, app)
  if opt == nil or opt == "" then
    return app
  else
    return opt .. ',' .. app
  end
end

if Fn.has('vim_starting') == 1 then -- Encoding
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
  fillchars    = { 'o', appendWithComma(vim.o.fillchars, 'fold:-') },
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
  ruler       = { 'o', false },
  showmode    = { 'o', false },
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

local function ftAuCmd (ftT, optT)
  local ftS = table.concat(ftT, ',')
  local optS = table.concat(optT, ' ')

  return 'autocmd FileType ' .. ftS .. ' setl ' .. optS
end

Cmd('augroup filetype_indentation')
  -- TODO: add preserveindent and copyindent for tabs
  Cmd('autocmd!')
    Cmd(ftAuCmd( -- Only Spaces Four Characters
      { 'python', 'matlab', 'help', 'lisp', 'haskell', 'lhaskell' },
      { 'tabstop=4', 'softtabstop=4', 'expandtab', 'shiftwidth=4' }))
    Cmd(ftAuCmd( -- Only Spaces Two Characters
      { 'vim', 'sh', 'zsh', 'markdown', 'tex', 'plaintex', 'bib', 'text', 'yaml',
        'ruby', 'xml', 'xdefaults', 'tmux', 'cabal', 'nix', 'lua' },
      { 'tabstop=2', 'softtabstop=2', 'expandtab', 'shiftwidth=2' }))
    Cmd(ftAuCmd( -- Tabs With Two Characters:
      { 'make', 'json', 'html', 'css', 'javascript', 'neomuttrc', 'muttrc' },
      { 'tabstop=2', 'softtabstop=2', 'noexpandtab', 'shiftwidth=2' }))
    Cmd(ftAuCmd( -- Tabs With Four Characters:
      { 'go', 'c', 'cpp', 'h' },
      { 'tabstop=4', 'softtabstop=4', 'noexpandtab', 'shiftwidth=4' }))
    -- C Indentation:
    Cmd(ftAuCmd(
      { 'c', 'cpp', 'h' },
      { 'cindent' }))
Cmd('augroup END')

Set_opt { -- Folding
  foldenable     = { 'w', true },
  foldcolumn     = { 'w', '1' },
  foldlevelstart = { 'o', 0 },
  foldmarker     = { 'w', '{{{,}}}' },
  foldnestmax    = { 'w', 10 },
}

Cmd('augroup fyletype_folding_method')
  Cmd('autocmd!')
  Cmd(ftAuCmd( -- Markers
    { 'vim', 'tex', 'plaintex', 'text', 'neomuttrc', 'muttrc', 'tmux' },
    { 'foldmethod=marker' }))
  Cmd(ftAuCmd( -- Indent
    { 'make', 'python', 'bib', 'go', 'json', 'html', 'css', 'javascript', 'yaml',
     'ruby', 'xml', 'haskell', 'lhaskell', 'cabal', 'nix', 'lua', 'lisp' },
    { 'foldmethod=indent' }))
  Cmd(ftAuCmd( -- Syntax
    { 'c', 'cpp', 'git', 'gitcommit', 'zsh', 'sh' },
    { 'foldmethod=syntax' }))
Cmd('augroup END')

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
  updatetime  = { 'o', 4000 },
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
