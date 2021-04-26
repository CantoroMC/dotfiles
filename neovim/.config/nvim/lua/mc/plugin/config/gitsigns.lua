require('gitsigns').setup {
  signs = {
    add = {
      hl = 'GitSignsAdd',
      text = '│',
      numhl='GitSignsAddNr',
      linehl='GitSignsAddLn'
    },
    change = {
      hl = 'GitSignsChange',
      text = '│',
      numhl='GitSignsChangeNr',
      linehl='GitSignsChangeLn'
    },
    delete = {
      hl = 'GitSignsDelete',
      text = '_',
      numhl='GitSignsDeleteNr',
      linehl='GitSignsDeleteLn'
    },
    topdelete = {
      hl = 'GitSignsDelete',
      text = '‾',
      numhl='GitSignsDeleteNr',
      linehl='GitSignsDeleteLn'
    },
    changedelete = {
      hl = 'GitSignsChange',
      text = '~',
      numhl='GitSignsChangeNr',
      linehl='GitSignsChangeLn'
    },
  },
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer  = true,
    -- motions
    ['n ]c'] = { expr = true, "&diff ? ']c' : '<Cmd>lua require\"gitsigns\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<Cmd>lua require\"gitsigns\".prev_hunk()<CR>'"},
    -- Text objects
    ['o ig'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    ['x ig'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    -- git actions
    ['n <C-C><C-H><C-S>'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <C-C><C-H><C-U>'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <C-C><C-H><C-R>'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['n <C-C><C-H><C-F>'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <C-C><C-H><C-P>'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <C-C><C-H><C-B>'] = '<cmd>lua require"gitsigns".blame_line()<CR>',

  },
  watch_index = {
    interval = 1000
  },
  numhl              = false,
  linehl             = false,
  current_line_blame = false,
  sign_priority      = 6,
  status_formatter   = nil,
  use_decoration_api = true,
  use_internal_diff  = true,
}
