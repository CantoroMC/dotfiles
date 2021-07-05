local opt = vim.opt



-- SECTION: Auxiliary functions

local function filetype_augroup(autocmd_table) -- {{{1
  for name, def in pairs(autocmd_table) do
    vim.cmd('augroup ' .. name )
    vim.cmd('autocmd!')
    for _, args in pairs(def) do
      vim.cmd(
        string.format(
          'autocmd FileType %s %s',
          vim.fn.join(args[1], ','),
          args[2]
        )
      )
    end
    vim.cmd('augroup END')
  end
end
-- }}}



-- SECTION: General Settings

-- ENCODING: {{{1
if vim.fn.has"vim_starting" then
  opt.encoding      = "utf-8"
  opt.fileencodings = { "ucs-bom", "utf-8", "default", "latin1" }
  opt.fileformats   = { "unix", "dos" }
end
-- }}}

-- TEXT: {{{1
opt.ambiwidth    = "single"
opt.backspace    = { "indent", "eol", "start" }
opt.conceallevel = 2
opt.fillchars    = { fold = "•", eob = "~" }
opt.emoji        = true
opt.list         = true
opt.listchars    = {
  eol      = "¬",
  extends  = "→",
  nbsp     = "+",
  precedes = "←",
  tab      = "┊·",
  trail    = "·",
}
opt.matchtime    = 5
opt.nrformats    = { "bin", "hex" }
opt.scrolloff    = 3
opt.showmatch    = true
opt.startofline  = true
opt.synmaxcol    = 3000
opt.textwidth    = 80
opt.virtualedit  = ""

opt.colorcolumn:append("80")
opt.iskeyword:append("-")
opt.matchpairs:append('<:>')

-- WRAPPING: {{{2
opt.breakindent    = true
opt.breakindentopt = "min:10,shift:4,sbr"
opt.linebreak      = true
opt.sidescroll     = 5
opt.sidescrolloff  = 2
opt.showbreak      = [[↪\]]
opt.whichwrap      = "b,s"
opt.wrap           = true
-- }}}
-- }}}

-- GENERIC: {{{1
opt.belloff       = "all"
opt.errorbells    = false
opt.visualbell    = false
opt.confirm       = true
opt.modelines     = 2
opt.mouse         = "a"
opt.lazyredraw    = false
opt.termguicolors = true
opt.timeout       = true
opt.timeoutlen    = 1000
opt.ttimeout      = true
opt.ttimeoutlen   = 50

opt.clipboard:append("unnamed")
opt.cpoptions:append("$")
opt.diffopt:append("vertical,context:2,iwhite")
-- }}}

-- SIGNCOLUMN: {{{1
opt.number         = true
opt.numberwidth    = 1
opt.relativenumber = true
opt.signcolumn     = "auto"
-- }}}

-- COMMANDLINE: {{{1
opt.cedit          = "<Home>"
opt.cmdheight      = 1
opt.inccommand     = "split"
opt.shortmess      = "atTcoOF"
opt.showcmd        = true
opt.wildchar       = 9 -- <Tab>
opt.wildcharm      = 9 -- <Tab>
opt.wildignorecase = true
opt.wildmenu       = true
opt.wildmode       = "longest:full,full"
opt.wildoptions    = "pum"

opt.wildignore:append("*~,*.o,*.obj,*.aux,*.git,*.svn,*.pyc,*pycache*,__pycache__")
-- }}}

-- STATUS_TAB_LINES: {{{1
opt.laststatus  = 2
opt.ruler       = true
opt.showmode    = false
opt.showtabline = 1
-- }}}

-- WINDOWS: {{{1
opt.equalalways = true
opt.splitbelow  = false
opt.splitright  = true
-- }}}

-- INDENTATION: {{{1
opt.shiftround = true
opt.autoindent = true
opt.smartindent = true

-- filetype_indentation {{{2
local two_spaces       = { -- {{{3
  'bib',
  'cabal',
  'lua',
  'markdown',
  'nix',
  'plaintex',
  'ruby',
  'sh',
  'sshconfig',
  'tex',
  'text',
  'tmux',
  'vim',
  'xdefaults',
  'xml',
  'yaml',
  'zsh',
}
-- }}}
local four_spaces      = { -- {{{3
  "conf",
  "config",
  "haskell",
  "help",
  "lhaskell",
  "lisp",
  "matlab",
  "python",
  "xf86conf",
}
-- }}}
local tab_two_spaces   = { -- {{{3
  'css',
  'html',
  'javascript',
  'json',
  'make',
  'muttrc',
  'neomuttrc',
}
-- }}}
local tab_three_spaces = { -- {{{3
  'c',
  'cpp',
  'h',
}
-- }}}
local tab_four_spaces  = { -- {{{3
  'go',
  'gitconfig',
  'ledger',
}
-- }}}

local indentation_table = { -- {{{3
  -- TODO: add preserveindent and copyindent for tabs
  { two_spaces,       "setlocal tabstop=2 softtabstop=2 expandtab   shiftwidth=2" },
  { four_spaces,      "setlocal tabstop=4 softtabstop=4 expandtab   shiftwidth=4" },
  { tab_two_spaces,   "setlocal tabstop=2 softtabstop=2 noexpandtab shiftwidth=2" },
  { tab_three_spaces, "setlocal tabstop=3 softtabstop=3 noexpandtab shiftwidth=3 cindent" },
  { tab_four_spaces,  "setlocal tabstop=4 softtabstop=4 noexpandtab shiftwidth=4" },
}
-- }}}

filetype_augroup{filetype_indentation = indentation_table}
-- }}}
-- }}}

-- FOLDING: {{{1
opt.foldenable     = true
opt.foldcolumn     = "auto:1"
opt.foldlevelstart = 0
opt.foldmarker     = "{{{,}}}"
opt.foldnestmax    = 10
opt.foldtext       = 'fooldme#foldtext()'

-- filetype_folding_method {{{2
local markers     = { -- {{{3
  'muttrc',
  'neomuttrc',
  'text',
  'tmux',
}
-- }}}
local indentation = { -- {{{3
  'bib',
  'c',
  'cabal',
  'conf',
  'config',
  'cpp',
  'css',
  'go',
  'haskell',
  'html',
  'javascript',
  'json',
  'lhaskell',
  'lisp',
  'lua',
  'make',
  'nix',
  'plaintex',
  'python',
  'ruby',
  'sh',
  'sshconfig',
  'tex',
  'vim',
  'xf86conf',
  'xml',
  'yaml',
  'zsh',
}
-- }}}
local syntax      = { -- {{{3
  'git',
  'gitcommit',
}
-- }}}
local treesitter  = {
  -- 'bash',
  -- 'c',
  -- 'cpp',
  -- 'go',
  'ledger',
  -- 'lua',
  -- 'nix',
  -- 'python',
  -- 'ruby',
  -- 'tex',
}

local foldmethod_table = { -- {{{3
  { markers,     "setlocal foldmethod=marker" },
  { indentation, "setlocal foldmethod=indent" },
  { syntax,      "setlocal foldmethod=syntax" },
  { treesitter,  "setlocal foldmethod=expr   foldexpr=nvim_treesitter#foldexpr()" },
}
-- }}}

filetype_augroup{filetype_folding_method = foldmethod_table}
-- }}}
-- }}}

-- FILES_BUFFERS: {{{1
opt.autoread  = true
opt.autowrite = true
opt.hidden    = true
opt.switchbuf = "useopen,usetab"

opt.path:append("**")
-- }}}

-- SPELLING: {{{1
opt.langremap    = false
opt.spelllang    = "en,it"
opt.spellfile    = vim.fn.stdpath('data') .. "/site/spell/dictionary.utf-8.add"
opt.spelloptions = "camel"
opt.spell        = false

-- Enable Spelling
filetype_augroup{
  spell_for_textual = {
    {
      {
        "bibtex",
        "context",
        "gitcommit",
        "markdown",
        "plaintex",
        "text",
      },
      "setlocal spell"
    }
  }
}
-- }}}

-- INSERT_MODE_COMPLETION: {{{1
opt.completeopt = { "menuone", "noinsert", "noselect" }
opt.omnifunc    = "syntaxcomplete#Complete"
opt.showfulltag = true

opt.complete:append("i,kspell")
opt.dictionary:append("spell,/usr/share/dict/cracklib-small")
-- }}}

-- POPUPMENU: {{{1
opt.pumblend  = 30
opt.pumheight = 20
-- }}}

-- SAFETY: {{{1
opt.backup      = false
opt.swapfile    = true
opt.updatetime  = 300
opt.writebackup = true
-- }}}

-- HISTORY: {{{1
opt.undodir        = vim.fn.stdpath('data') .. "/undo"
opt.undofile       = true
opt.history        = 200
opt.sessionoptions = { "blank", "buffers", "curdir", "folds", "tabpages", "winsize" }
opt.viewdir        = vim.fn.stdpath('data') .. "/view"
opt.viewoptions    = { "cursor", "curdir", "folds", "options" }
-- }}}

-- SEARCHING: {{{1
if vim.fn.executable('rg') then
  vim.opt.grepprg="rg -s --hidden --vimgrep"
elseif vim.fnexecutable('ag') then
  vim.opt.grepprg="ag -sR --hidden --vimgrep"
elseif vim.fn.has('unix') then
  vim.opt.grepprg="grep -rRHnIi $* /dev/null"
end

opt.grepformat   = {"%f:%l:%c:%m", "%f:%l:%m", "%f:%l%m", "%f %l%m"}
opt.hlsearch     = true
opt.ignorecase   = true
opt.incsearch    = true
opt.regexpengine = 0
opt.smartcase    = true
opt.tagcase      = "followscs"
-- }}}

vim.cmd'filetype plugin indent on'
vim.cmd'syntax enable'



-- SECTION: ftplugin settings and more autocommands

-- FORMATOPTIONS: {{{1
-- CHEATSHEET: {{{2
-- jcroql is the default.
------------------------------------------------------------------------------"
--|  t | Auto-wrap text using textwidth                                       |
--|  c | Auto-wrap comments using textwidth, inserting the current            |
--|        comment leader automatically                                       |
--|  r | Automatically insert the current comment leader after                |
--|        hitting <Enter> in Insert mode                                     |
--|  o | Automatically insert the current comment leader after                |
--|        hitting o in Insert mode                                           |
--|  q | Allow formatting of comments with "gq".                              |
--|  w | Trailing white space indicates a paragraph continues in the next line| .
--|        A line that ends in a non-white character ends a paragraph.        |
--|  a | Automatic formatting of paragraphs.                                  |
--|  n | When formatting text, recognize numbered lists                       |
--|  2 | When formatting text, use the indent of the second line of a         |
--|        paragraph for the rest of the paragraph, instead of the indent of  |
--|        the first line.                                                    |
--|  v | Vi-compatible auto-wrapping in insert mode: Only break a line at a   |
--|        blank that you have entered during the current insert command.     |
--|  l | Long lines are not broken in insert mode                             |
--|  m | Also break at a multi-byte character above 255.                      |
--|  B | When joining lines, don't insert a space between two                 |
--|        multi-byte characters                                              |
--|  1 | Don't break a line after a one-letter word.  It's broken before it   |
--|        instead.                                                           |
--|  j | Where it make sense, remove a comment leader when joining lines      |
------------------------------------------------------------------------------"
-- }}}
vim.cmd[[
  augroup user_formatoptions')
  autocmd!
  autocmd Filetype * setlocal formatoptions-=o
  autocmd Filetype * setlocal formatoptions-=t
  autocmd Filetype * setlocal formatoptions+=nB
  augroup END
]]
-- }}}

-- COMMENTSTRING: {{{1
filetype_augroup{
  comment_strings = {
    { { "c", "cpp", "cs" }, [[setlocal commentstring=//\ %s]] },
    { { "desktop", },       [[setlocal commentstring=#\ %s]] },
    { { "cabal", },         [[setlocal commentstring=--\ %s]] },
  }
}
-- }}}

-- KEYWORDPRG: {{{1
filetype_augroup{
  keywordprg_filetype = {
    { { "ruby" },     [[setlocal keywordprg=ri\ --format=markdown]] },
    { { "haskell", }, [[setlocal keywordprg=hoogle\ -q\ --info]] },
  }
}
-- }}}

-- HIGHLIGHTONYANK: {{{1
vim.cmd[[
  augroup user_formatoptions'
  autocmd!
  autocmd TextYankPost * lua vim.highlight.on_yank({higroup="IncSearch", on_visual = false})
  augroup END
]]
-- }}}



-- SECTION: Distribution Plugins And Scripts Options

-- Disable Unwanted Plugins {{{1
vim.g.loaded_2html_plugin      = true
vim.g.loaded_gzip              = true
vim.g.loaded_tar               = true
vim.g.loaded_tarPlugin         = true
vim.g.loaded_zip               = true
vim.g.loaded_zipPlugin         = true
vim.g.loaded_tutor_mode_plugin = true
-- }}}

-- Add Optional Packages {{{1

-- Terminal debugger
vim.cmd'packadd! termdebug'
vim.g.termdebug_wide = opt.columns:get()/2
-- vim.g.termdebug_disasm_window = 5

-- Text justification
vim.cmd'packadd! justify'

-- Quickfix list filtering
vim.cmd'packadd! cfilter'

-- Make life easier with plugins
-- vim.cmd'packadd! vimball'
-- }}}

-- Script Variables {{{1

-- Plugins: {{{2
-- Matchit
vim.b.match_ignorecase = 1
-- }}}

-- Syntax Files {{{2

-- Lisp {{{3
vim.g.lisp_rainbow = 1
vim.g.lisp_instring = 1
vim.g.lispsyntax_clisp = 1
-- }}}

-- Markdown: {{{3
vim.g.markdown_fenced_languages = {
  'bash=sh',
  'c',
  'cpp',
  'help',
  'html',
  'matlab',
  'python',
  'tex',
  'vim',
}
-- }}}

-- Python: {{{3
vim.cmd'let python_highlight_all = 1'
-- }}}

-- Tex {{{3
-- Conceal Options {{{4
-- ---------------------------------
-- | a |   accents/ligatures        |
-- | b |   bold and italic          |
-- | d |   delimiters               |
-- | m |   math symbols             |
-- | g |   Greek                    |
-- | s |   superscripts/subscripts  |
-- ---------------------------------
-- }}}
vim.g.tex_conceal = 'abdmgs'
vim.g.tex_flavor = 'latex'
-- }}}

-- Vim {{{3
-- Embedded script highlighting supported ((Lua,Py,Ruby) or 0 for disabling)
vim.g.vimsyn_embed = 'lPr'
-- folding for syntax (augroup, fold fun, Python)
vim.g.vimsyn_folding = 'afP'
-- }}}

-- Shell And Zsh {{{3
vim.g.sh_fold_enabled= 7
-- let g:zsh_fold_enable = 1
-- }}}

-- }}}

-- }}}


-- vim:fdm=marker
