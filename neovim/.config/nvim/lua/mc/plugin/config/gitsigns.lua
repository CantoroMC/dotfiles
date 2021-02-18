require('gitsigns').setup {
  signs = {
    add          = {hl = 'DiffAdd'   , text = '│', numhl='GitSignsAddNr'},
    change       = {hl = 'DiffChange', text = '│', numhl='GitSignsChangeNr'},
    delete       = {hl = 'DiffDelete', text = '_', numhl='GitSignsDeleteNr'},
    topdelete    = {hl = 'DiffDelete', text = '‾', numhl='GitSignsDeleteNr'},
    changedelete = {hl = 'DiffChange', text = '~', numhl='GitSignsChangeNr'},
  },
  numhl = true,
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<Cmd>lua require\"gitsigns\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<Cmd>lua require\"gitsigns\".prev_hunk()<CR>'"},

    ['n <Leader>hs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <Leader>hu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <Leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['n <Leader>hp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <Leader>hb'] = '<cmd>lua require"gitsigns".blame_line()<CR>',

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns".text_object()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns".text_object()<CR>'
  },
  watch_index = {
    interval = 1000
  },
  sign_priority = 6,
  status_formatter = nil, -- Use default
}
