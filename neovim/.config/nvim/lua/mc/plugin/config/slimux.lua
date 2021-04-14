vim.api.nvim_set_keymap('n', '<C-c><C-s>l',
  '<Cmd>SlimuxREPLSendLine<CR>',      { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-c><C-s>b',
  '<Cmd>SlimuxREPLSendBuffer<CR>',    { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-c><C-s>p',
  '<Cmd>SlimuxREPLSendParagraph<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-c><C-s>',
  '<Cmd>SlimuxREPLSendSelection<CR>', { noremap = true, silent = true })
