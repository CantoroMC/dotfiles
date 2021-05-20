vim.g.undotree_WindowLayout      = 4
vim.g.undotree_SplitWidth        = 22
vim.g.undotree_DiffpanelHeight   = 8
vim.g.undotree_DiffAutoOpen      = 1
vim.g.undotree_RelativeTimestamp = 1
vim.g.undotree_TreeNodeShape     = " "
vim.g.undotree_ShortIndicators   = 1
vim.g.undotree_HelpLine          = 0

vim.api.nvim_set_keymap('n', '<C-c><C-u>',
  '<Cmd>UndotreeToggle<CR>', { noremap = true, silent = true })
