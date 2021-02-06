-- Mappings

--[[ Cheat Table

   Commands \ Modes | Api |Normal|Insert|Command|Visual|Select|Operator|Lang-Arg|Terminal|
  =======================================================================================
   map  / noremap   | ''  |   @  |  -   |   -   |  @   |  @   |   @    |   -    |   -    |
   nmap / nnoremap  | 'n' |   @  |  -   |   -   |  -   |  -   |   -    |   -    |   -    |
   vmap / vnoremap  | 'v' |   -  |  -   |   -   |  @   |  @   |   -    |   -    |   -    |
   xmap / xnoremap  | 'x' |   -  |  -   |   -   |  @   |  -   |   -    |   -    |   -    |
   smap / snoremap  | 's' |   -  |  -   |   -   |  -   |  @   |   -    |   -    |   -    |
   omap / onoremap  | 'o' |   -  |  -   |   -   |  -   |  -   |   @    |   -    |   -    |
   map! / noremap!  | '!' |   -  |  @   |   @   |  -   |  -   |   -    |   -    |   -    |
   imap / inoremap  | 'i' |   -  |  @   |   -   |  -   |  -   |   -    |   -    |   -    |
   lmap / lnoremap  | 'l' |   -  |  @   |   @   |  -   |  -   |   -    |   @    |   -    |
   cmap / cnoremap  | 'c' |   -  |  -   |   @   |  -   |  -   |   -    |   -    |   -    |
   tmap / tnoremap  | 't' |   -  |  -   |   @   |  -   |  -   |   -    |   -    |   @    |

-- ]]

Set_var { -- Leader And LocalLeader
  mapleader      = { 'g', ',' },
  maplocalleader = { 'g', ' ' },
}

Set_map { -- Reimplemented Keys
  { 'n', 'Q',     '@@',         {noremap = true} }, -- Repeat the previous macro.
  { 'n', 'ZQ',    ':wqall<CR>', {noremap = true} }, -- Disable risky :q!
  { 'n', '<C-Q>', ':q<CR>',     {noremap = true} }, -- Closing
  { '',  '<C-J>', 'G',          {noremap = true} }, -- Movement with control
  { '',  '<C-K>', 'gg',         {noremap = true} },
  { '',  '<C-H>', '^',          {noremap = true} },
  { '',  '<C-L>', '$',          {noremap = true} },
  { '',  '<C-S>', '<C-L>',      {noremap = true} }, -- Move :mode | redraw! to <C-S>
  { 'i', 'jk',    '<Esc>',      {noremap = true} }, -- Exit insert mode with ease
}

--[[ Cheat Table

   Key      | Action
   =============================================================================|
   c_CTRL-A | All names that match the pattern in front the cursor are inserted.|
   c_CTRL-E | Cursor to end of command-line                                     |
   c_CTRL-B | Cursor to beginning of command-line                               |
   c_CTRL-D | List names that match the pattern in front of the cursor          |
   c_CTRL-F | Default cedit key, used to open the command-line window           |
   =========|===================================================================|
   c_CTRL-N | After 'wildchar' which got multiple matches, go to next match.    |
            |   Otherwise recall more recent command-line from history.         |
   c_CTRL-P | After 'wildchar' which got multiple matches, go to previous match.|
            |   Otherwise recall older command-line from history.               |
   c_<Up>   | Recall older command-line from history.                           |
   c_<Up>   | Recall more recent command-line from history.                     |

--]]

Set_map { -- Command Mode Reimplementation
  { 'c', '<C-A>',     '<C-B>',   {noremap = true} }, -- Emacs Like Movement In Command Mode:
  { 'c', '<C-E>',     '<End>',   {noremap = true} },
  { 'c', '<C-D>',     '<Del>',   {noremap = true} },
  { 'c', '<C-B>',     '<Left>',  {noremap = true} },
  { 'c', '<C-F>',     '<Right>', {noremap = true} },
  { 'c', '<C-P>',     '<Up>',    {noremap = true} }, -- Better Control P And N In Command Mode:
  { 'c', '<C-N>',     '<Down>',  {noremap = true} },
  { 'c', '<Up>',      '<C-P>',   {noremap = true} },
  { 'c', '<Down>',    '<C-N>',   {noremap = true} },
  { 'c', '<C-R><C-L>', 
      [[<C-R>=substitute(getline("."), "^\s*", "", "")<CR>]],
                                 {noremap = true} }, -- Send line to command line
}

Set_map { -- Windows
  { 'n', '<Leader>h', ':<C-U>split <bar> bp<CR>',               -- Window Splitting
    {noremap = true, silent = true} },
  { 'n', '<Leader>v', ':<C-U>vsplit <bar> bp<CR>',
    {noremap = true, silent = true} },
  { '',  '<S-Left>', [[:<C-U>execute v:count1.'wincmd <'<CR>]], -- Window Resizing
    {noremap = true, silent = true} },
  { '',  '<S-Right>', [[:<C-U>execute v:count1.'wincmd >'<CR>]],
    {noremap = true, silent = true} },
  { '',  '<S-Up>', [[:<C-U>execute v:count1.'wincmd +'<CR>]],
    {noremap = true, silent = true} },
  { '',  '<S-Down>', [[:<C-U>execute v:count1.'wincmd -'<CR>]],
    {noremap = true, silent = true} },
  { 'c', 'wbd', 'write <Bar> bdelete',
    {noremap = true} },
}

Set_map { -- Tabs
  { 'n', '<C-W>t',     ':<C-U>tabnew<CR>',   {noremap = true, silent = true} },
  { 'n', '<C-W><C-T>', ':<C-U>tabnew<CR>',   {noremap = true, silent = true} },
  { 'n', '<C-W>Q',     ':<C-U>tabclose<CR>', {noremap = true, silent = true} },
  { 'n', '<C-N>',      'gt',                 {noremap = true, silent = true} },
  { 'n', '<C-P>',      'gT',                 {noremap = true, silent = true} },
  { 'n', 'gtf',         [[:<C-U>execute 'tabnew'.expand('<cfile>:p')<CR>]],
                                             {noremap = true, silent = true} }, -- Tab version of gf
  { 'n', '<C-]><C-T>', '<C-W><C-]><C-W>T',   {noremap = true} },                -- Tab version `<C-]>`.
}

-- Set_map { -- Some Editing Tricks
-- }
