local appendWithComma = function(opt, app)
  if opt == nil or opt == "" then
    return app
  else
    return opt .. ',' .. app
  end
end

local ftAuCmd = function(ftT, optT)
  local ftS = table.concat(ftT, ',')
  local optS = table.concat(optT, ' ')

  return 'autocmd FileType ' .. ftS .. ' setl ' .. optS
end

local function set_opt(opts_table)
  for k,v in pairs(opts_table) do
    vim.api.nvim_set_option(k, v)
  end
end

vim.api.nvim_set_option

-- General Settings: {{{1

-- Encoding: {{{2
if vim.fn.has('vim_starting') == 1 then
  vim.g.encoding      = 'utf-8'
  vim.o.fileencodings = 'ucs-bom,utf-8,default,latin1'
  vim.o.fileformats  = 'unix,dos'
end

-- }}}

-- Text: {{{2

-- set_opt {
--   'ambiwidth' = 'single'
-- }
vim.o.ambiwidth     = 'single'
vim.o.backspace     = 'indent,eol,start'
vim.wo.colorcolumn  = appendWithComma(vim.wo.colorcolumn, '80')
vim.wo.conceallevel = 2
vim.o.emoji         = true
vim.o.fillchars     = appendWithComma(vim.o.fillchars, 'fold:-')
vim.bo.iskeyword    = appendWithComma(vim.bo.iskeyword, '-')
vim.wo.list         = true
vim.o.listchars     = 'tab:>-,trail:·,nbsp:+,eol:¬,precedes:←,extends:→'
vim.bo.matchpairs   = appendWithComma(vim.bo.matchpairs, '<:>')
vim.o.matchtime     = 5
vim.bo.nrformats    = 'bin,hex'
vim.o.scrolloff     = 3
vim.o.showmatch     = true
vim.o.startofline   = true
vim.bo.synmaxcol    = 3000
vim.bo.textwidth    = 80
vim.o.virtualedit   = ""

-- Wrapping: {{{3
vim.wo.breakindent    = true
vim.wo.breakindentopt = 'min:10,shift:4,sbr'
vim.wo.linebreak      = true
vim.o.sidescroll      = 5
vim.o.sidescrolloff   = 2
vim.o.showbreak       = [[↪\]]
vim.o.whichwrap       = 'b,s'
vim.wo.wrap           = true
-- }}}

-- }}}

-- Generic: {{{2

vim.o.belloff       = 'all'
vim.o.errorbells    = false
vim.o.visualbell    = false
vim.o.clipboard     = appendWithComma(vim.o.clipboard, 'unnamed')
vim.o.confirm       = true
vim.o.cpoptions     = vim.o.cpoptions .. '$'
vim.o.diffopt       = appendWithComma(vim.o.diffopt, 'vertical,context:2,iwhite')
vim.o.mouse         = 'a'
vim.o.lazyredraw    = false
-- set_option('termguicolors', true)
vim.o.termguicolors = true
vim.o.timeout       = true
vim.o.timeoutlen    = 1000
vim.o.ttimeout      = true
vim.o.ttimeoutlen   = 50

-- }}}

-- Sign Column: {{{2

vim.wo.number         = true
vim.wo.numberwidth    = 1
vim.wo.relativenumber = true
vim.wo.signcolumn     = 'auto'

-- }}}

-- Command Line: {{{2

vim.o.cedit          = '<Home>'
vim.o.cmdheight      = 2
vim.o.inccommand     = 'split'
vim.o.shortmess      = 'atTcoOF'
vim.o.showcmd        = true
vim.o.wildchar       = 9 -- corresponding to <Tab>
vim.o.wildcharm      = 9
vim.o.wildignore     = appendWithComma(vim.o.wildignore,
                        '*~,*.o,*.obj,*.aux,*.git,*.svn,*.pyc,__pycache__')
vim.o.wildignorecase = true
vim.o.wildmenu       = true
vim.o.wildmode       = 'longest:full,full'
vim.o.wildoptions    = 'pum'

-- }}}

-- Status And Tab Lines: {{{2

vim.o.laststatus  = 2
vim.o.ruler       = false
vim.o.showmode    = false
vim.o.showtabline = 1

-- }}}

-- Windows: {{{2

vim.o.splitbelow = true
vim.o.splitright = true

-- }}}

-- Indentation: {{{2

vim.o.shiftround = true
vim.bo.autoindent = true
vim.bo.smartindent = true

vim.cmd('augroup filetype_indentation') -- {{{3
  -- TODO: add preserveindent and copyindent for tabs
  vim.cmd('autocmd!')
    -- Only Spaces Four Characters:
    vim.cmd('autocmd FileType' .. ' ' ..
            'python,matlab,help,lisp,haskell,lhaskell' .. ' ' ..
            'setl tabstop=4 softtabstop=4 expandtab shiftwidth=4')
    -- Only Spaces Two Characters:
    vim.cmd('autocmd FileType' .. ' ' ..
            'vim,sh,zsh,markdown,tex,plaintex,bib,text,yaml,ruby,xml,xdefaults,tmux,cabal,nix,lua' .. ' ' ..
            'setl tabstop=2 softtabstop=2 expandtab shiftwidth=2')
    -- Tabs With Two Characters:
    vim.cmd('autocmd FileType' .. ' ' ..
            'make,json,html,css,javascript,neomuttrc,muttrc' .. ' ' ..
            'setl tabstop=2 softtabstop=2 noexpandtab shiftwidth=2')
    -- Tabs With Four Characters:
    vim.cmd('autocmd FileType ' .. ' ' ..
            'go,c,cpp,h' .. ' ' ..
            'setl tabstop=4 softtabstop=4 noexpandtab shiftwidth=4')
    -- C Indentation:
    vim.cmd('autocmd FileType' .. ' ' ..
            'c,cpp,h' .. ' ' ..
            'setl cindent')
vim.cmd('augroup END')
-- }}}

-- }}}

-- Folding: {{{2

vim.wo.foldenable    = true
vim.wo.foldcolumn    = '1'
vim.o.foldlevelstart = 0
vim.wo.foldmarker    = '{{{,}}}'
vim.wo.foldnestmax   = 10

vim.cmd('augroup fyletype_folding_method') -- {{{3
  vim.cmd('autocmd!')
  vim.cmd(ftAuCmd(
    { 'vim', 'tex', 'plaintex', 'text', 'neomuttrc', 'muttrc', 'tmux' },
    { 'foldmethod=marker' }))
  vim.cmd(ftAuCmd(
    { 'make', 'python', 'bib', 'go', 'json', 'html', 'css', 'javascript', 'yaml',
     'ruby', 'xml', 'haskell', 'lhaskell', 'cabal', 'nix', 'lua', 'lisp' },
    { 'foldmethod=indent' }))
  vim.cmd(ftAuCmd(
    { 'c', 'cpp', 'git', 'gitcommit', 'zsh', 'sh' },
    { 'foldmethod=syntax' }))
vim.cmd('augroup END')
-- }}}

-- -- }}}

-- -- Files And Buffers: {{{2

-- set autoread
-- set autowrite
-- set hidden
-- set path+=**
-- set switchbuf=useopen

-- -- }}}

-- -- Spelling: {{{2

-- set nolangremap
-- set spelllang=en,it
-- set spellfile=$XDG_CONFIG_HOME/nvim/spell/dictionary.utf-8.add
-- set spell

-- -- }}}

-- -- Insert Mode Completion: {{{2

-- set complete+=i,kspell
-- set completeopt=menuone,preview,noinsert,noselect
-- set dictionary+="spell"
-- set omnifunc=syntaxcomplete#Complete
-- set showfulltag

-- -- }}}

-- -- Pop Up Menu: {{{2

-- if has('nvim')
--   set pumblend=30
-- endif
-- set pumheight=20

-- -- }}}

-- -- Safety: {{{2

-- set nobackup
-- set swapfile
-- set updatetime=500
-- set writebackup

-- -- }}}

-- -- History: {{{2

-- set undodir+="$XDG_DATA_HOME/nvim/undo"
-- set undofile
-- set history=200
-- set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
-- set viewdir+="$XDG_DATA_HOME/nvim/view"
-- set viewoptions=cursor,curdir,folds,options

-- -- }}}

-- -- Searching: {{{2

-- set grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ %l%m
-- if executable('rg')
--   set grepprg=rg\ -s\ --hidden\ --vimgrep
-- elseif executable('ag')
--   set grepprg=ag\ -sR\ --hidden\ --vimgrep
-- elseif has('unix')
--   set grepprg=grep\ -rRHnIi\ $*\ /dev/null
-- endif
-- set hlsearch
-- set ignorecase
-- set incsearch
-- set regexpengine=0
-- set smartcase
-- set tagcase=followscs

-- }}}

-- }}}


require('plugins')

-- Mappings: {{{1

-- Cheat Table: {{{2

-- ---------------------------------------------------------------------------------------
-- Commands \ Modes | Api |Normal|Insert|Command|Visual|Select|Operator|Lang-Arg|Terminal|
-- -----------------|-----|------|------|-------|------|------|---------------------------
-- map  / noremap   | ''  |   @  |  -   |   -   |  @   |  @   |   @    |   -    |   -    |
-- nmap / nnoremap  | 'n' |   @  |  -   |   -   |  -   |  -   |   -    |   -    |   -    |
-- vmap / vnoremap  | 'v' |   -  |  -   |   -   |  @   |  @   |   -    |   -    |   -    |
-- xmap / xnoremap  | 'x' |   -  |  -   |   -   |  @   |  -   |   -    |   -    |   -    |
-- smap / snoremap  | 's' |   -  |  -   |   -   |  -   |  @   |   -    |   -    |   -    |
-- omap / onoremap  | 'o' |   -  |  -   |   -   |  -   |  -   |   @    |   -    |   -    |
-- map! / noremap!  | '!' |   -  |  @   |   @   |  -   |  -   |   -    |   -    |   -    |
-- imap / inoremap  | 'i' |   -  |  @   |   -   |  -   |  -   |   -    |   -    |   -    |
-- lmap / lnoremap  | 'l' |   -  |  @   |   @   |  -   |  -   |   -    |   @    |   -    |
-- cmap / cnoremap  | 'c' |   -  |  -   |   @   |  -   |  -   |   -    |   -    |   -    |
-- tmap / tnoremap  | 't' |   -  |  -   |   @   |  -   |  -   |   -    |   -    |   @    |
-- ---------------------------------------------------------------------------------------

-- }}}

-- Leader And LocalLeader {{{2

vim.g.mapleader = ','
vim.g.maplocalleader = ' '

-- }}}

-- Reimplemented Keys: {{{2

-- Repeat the previous macro.
vim.api.nvim_set_keymap('', 'Q', '@@', {noremap = true})
-- Disable risky :q!
vim.api.nvim_set_keymap('', 'ZQ', ':q!<CR>', {noremap = true})
-- Closing
vim.api.nvim_set_keymap('', '<C-Q>', ':q<CR>', {noremap = true})
-- Movement with control:
vim.api.nvim_set_keymap('', '<C-J>', 'G',  {})
vim.api.nvim_set_keymap('', '<C-K>', 'gg', {})
vim.api.nvim_set_keymap('', '<C-H>', '^',  {})
vim.api.nvim_set_keymap('', '<C-L>', '$',  {})
-- Move :mode | redraw! to <C-S>
vim.api.nvim_set_keymap('', '<C-S>', '<C-L>', {})

-- Exit insert mode with ease
vim.api.nvim_set_keymap('i', 'jk', '<Esc>', {})

-- Command Mode: {{{3

-- Cheat Table: {{{4

-- ------------------------------------------------------------------------------
-- Key      | Action                                                            |
-- ---------|-------------------------------------------------------------------|
-- c_CTRL-A | All names that match the pattern in front the cursor are inserted.|
-- c_CTRL-E | Cursor to end of command-line                                     |
-- c_CTRL-B | Cursor to beginning of command-line                               |
-- c_CTRL-D | List names that match the pattern in front of the cursor          |
-- c_CTRL-F | Default cedit key, used to open the command-line window           |
-- ---------|-------------------------------------------------------------------|
-- c_CTRL-N | After 'wildchar' which got multiple matches, go to next match.    |
--          |   Otherwise recall more recent command-line from history.         |
-- c_CTRL-P | After 'wildchar' which got multiple matches, go to previous match.|
--          |   Otherwise recall older command-line from history.               |
-- c_<Up>   | Recall older command-line from history.                           |
-- c_<Up>   | Recall more recent command-line from history.                     |
-- ------------------------------------------------------------------------------

-- }}}

-- Emacs Like Movement In Command Mode:
vim.api.nvim_set_keymap('c', '<C-A>', '<C-B>',   {noremap = true})
vim.api.nvim_set_keymap('c', '<C-E>', '<End>',   {noremap = true})
vim.api.nvim_set_keymap('c', '<C-D>', '<Del>',   {noremap = true})
vim.api.nvim_set_keymap('c', '<C-B>', '<Left>',  {noremap = true})
vim.api.nvim_set_keymap('c', '<C-F>', '<Right>', {noremap = true})
-- Better Control P And N In Command Mode:
vim.api.nvim_set_keymap('c', '<C-P>',  '<Up>',   {noremap = true})
vim.api.nvim_set_keymap('c', '<C-N>',  '<Down>', {noremap = true})
vim.api.nvim_set_keymap('c', '<Up>',   '<C-P>',  {noremap = true})
vim.api.nvim_set_keymap('c', '<Down>', '<C-N>',  {noremap = true})

-- Send line to command line
-- vim.api.nvim_set_keymap('c', '<C-R><C-L>', '<C-R>=substitute(getline("."), "^\s*", "", "")<CR>', {noremap = true})

-- }}}

-- }}}

-- }}}

vim.cmd('colorscheme ayu')
vim.g.ayu_dark_flavor = 'dark'

-- vim:fdm=marker
