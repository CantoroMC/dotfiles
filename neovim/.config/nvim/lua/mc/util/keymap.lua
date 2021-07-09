local keymap = vim.api.nvim_set_keymap

-- SECTION: Mappings

-- Leader And LocalLeader: {{{1
vim.g.mapleader = ","
vim.g.maplocalleader = " "
-- }}}

-- CHEATSHEET: {{{1
-- Commands \ Modes | Api |Normal|Insert|Command|Visual|Select|Operator|Lang-Arg|Terminal|
--=======================================================================================
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
-----------------------------------------------------------------------------------------
-- }}}



-- SECTION: Reimplemented Keys

-- Normal Visual Select Operator Modes {{{1
keymap("n", "Q",     "@@",             { noremap = true })  -- Repeat the previous macro.
keymap("n", "ZQ",    "<Cmd>wqall<CR>", { noremap = true })  -- Disable risky :q!
keymap("n", "<C-Q>", "<Cmd>q<CR>",     { noremap = true })
keymap("n", "<C-S>", "<C-L>",          { noremap = true })  -- Move :mode | redraw! with <C-S>
-- Movements with Control
keymap("n", "<C-J>", "G",  { noremap = true })
keymap("n", "<C-K>", "gg", { noremap = true })
keymap("n", "<C-H>", "^",  { noremap = true })
keymap("n", "<C-L>", "$",  { noremap = true })
-- }}}

-- Insert Mode {{{1
keymap("i", "jk", "<Esc>", { noremap = true })  -- Exit insert mode with ease
-- Emacs-like line navigation
keymap("i", "<M-a>", "<Home>",    { noremap = true })
keymap("i", "<M-e>", "<End>",     { noremap = true })
keymap("i", "<M-f>", "<S-Right>", { noremap = true })
keymap("i", "<M-b>", "<S-Left>",  { noremap = true })
keymap("i", "<M-p>", "<Up>",      { noremap = true })
keymap("i", "<M-n>", "<Down>",    { noremap = true })
-- }}}

-- Command Mode {{{1

-- CHEATSHEET: {{{2
-- ----------------------------------------------------------------------------"
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
-- -----------------------------------------------------------------------------"
-- }}}

-- Emacs Like Movement In Command Mode:
keymap("c", "<C-A>", "<C-B>",    { noremap = true })
keymap("c", "<C-E>", "<End>",    { noremap = true })
keymap("c", "<C-D>", "<Del>",    { noremap = true })
keymap("c", "<C-B>", "<Left>",   { noremap = true })
keymap("c", "<C-F>", "<Right>",  { noremap = true })
-- Better Control P And N In Command Mode:
keymap("c", "<C-P>",  "<Up>",   { noremap = true })
keymap("c", "<C-N>",  "<Down>", { noremap = true })
keymap("c", "<Up>",   "<C-P>",  { noremap = true })
keymap("c", "<Down>", "<C-N>",  { noremap = true })
keymap("c", "<C-R><C-L>",
  [[<C-R>=substitute(getline('.'), '^\s*', '', '')<CR>]],
  { noremap = true }) -- Send line to command line
-- }}}

-- Terminal Mode {{{1
keymap("t", "<C-R>", [['<C-\><C-N>"'.nr2char(getchar()).'pi']],
  { noremap = true, expr = true }) -- Simulate i_CTRL-R in terminal mode
-- Window navigation even in terminal mode
keymap("t", "<C-W>h", [[<C-\><C-N><C-w>h]], { noremap = true })
keymap("t", "<C-W>j", [[<C-\><C-N><C-w>j]], { noremap = true })
keymap("t", "<C-W>k", [[<C-\><C-N><C-w>k]], { noremap = true })
keymap("t", "<C-W>l", [[<C-\><C-N><C-w>l]], { noremap = true })
-- }}}



-- SECTION: Window And Tabs

-- Windows {{{1
-- Window Splitting
keymap("n", "<Leader>h", "<Cmd>split  <bar> bp<CR>", { noremap = true, silent = true })
keymap("n", "<Leader>v", "<Cmd>vsplit <bar> bp<CR>", { noremap = true, silent = true })
-- Window Resizing
keymap("", "<S-Left>",  ':<C-U>execute v:count1 "wincmd <"<CR>', { noremap = true, silent = true } )
keymap("", "<S-Right>", ':<C-U>execute v:count1 "wincmd >"<CR>', { noremap = true, silent = true } )
keymap("", "<S-Up>",    ':<C-U>execute v:count1 "wincmd +"<CR>', { noremap = true, silent = true } )
keymap("", "<S-Down>",  ':<C-U>execute v:count1 "wincmd -"<CR>', { noremap = true, silent = true } )
-- }}}

-- Tabs {{{1
keymap("n", "<C-W>t",     "<Cmd>tabnew<CR>",   { noremap = true, silent = true })
keymap("n", "<C-W><C-T>", "<Cmd>tabnew<CR>",   { noremap = true, silent = true })
keymap("n", "<C-W>Q",     "<Cmd>tabclose<CR>", { noremap = true, silent = true })
keymap("n", "<C-N>",      "gt",                { noremap = true, silent = true })
keymap("n", "<C-P>",      "gT",                { noremap = true, silent = true })
-- Tab version of gf
keymap("n", "gtf", "<C-W>gf", { noremap = true, silent = true })
-- Tab version of gF
keymap("n", "gtF", "<C-W>gF", { noremap = true, silent = true })
-- Tab version `<C-]>`.
keymap("n", "<C-]><C-T>", "<C-W><C-]><C-W>T", { noremap = true, silent = true })
-- }}}


-- SECTION: Editing

-- Some Editing Tricks {{{1
-- Substitutions with ease.
keymap("n", "<Leader>ra", ":%s///g<Left><Left><Left>", { noremap = true })
-- Swap two words.
keymap("n", "<Leader>sw", [["_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>]], { noremap = true })
-- Overwrite the current line with yanked text.
keymap("n", "go", 'pk"_dd', { noremap = true, silent = true })
-- Indent continuously.
keymap("v", "<", "<gv", { noremap = true })
keymap("v", ">", ">gv", { noremap = true })
-- Moving lines up and down.
keymap("v", "K", ":move '<-2<CR>gv=gv", { noremap = true })
keymap("v", "J", ":move '>+1<CR>gv=gv", { noremap = true })
-- Assist input normal command on visual mode.
keymap("v", "n", ":normal<Space>", { noremap = true })
-- Search something in the current visual range only.
keymap("v", "/", [[<Esc>/\%V]], { noremap = true })
-- Global substitution in visual mode.
keymap("v", "<C-S>", [["hy:%s/\V<C-R>h//g<left><left>]], { noremap = true })
-- }}}

-- vim:fdm=marker
