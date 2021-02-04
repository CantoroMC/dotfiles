
-- General Settings: {{{1

-- Text: {{{2

vim.o.ambiwidth = 'single'
vim.o.backspace = 'indent,eol,start'
vim.wo.colorcolumn = '80'
vim.wo.conceallevel = 2
vim.o.emoji = true
vim.o.fillchars = 'fold:-'

-- }}}


vim.wo.number = true



-- }}}

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
--
-- }}}
