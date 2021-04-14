vim.api.nvim_set_keymap('n', '<Leader>a/',
  '<Cmd>Tabularize ////l8c1l0<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>a=',
  '<Cmd>Tabularize /=/l1c1l0<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>a"',
  '<Cmd>Tabularize /"/l1c1l0<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>at',
  '<Cmd>Tabularize /<Bar><CR>', { noremap = true, silent = true })
