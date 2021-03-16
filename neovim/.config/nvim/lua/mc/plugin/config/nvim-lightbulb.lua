require'nvim-lightbulb'.update_lightbulb {
  sign = {
    enabled = true,
    priority = 10,
  },
  float = {
    enabled = false,
    text = "ﯦ ",
    win_opts = {},
  },
  virtual_text = {
    enabled = true,
    text = "ﯦ ",
  }
}

vim.fn.sign_define('LightBulbSign',
  { text = "ﯦ ", texthl = "String", linehl = "", numhl = ""})
vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]
vim.api.nvim_command('highlight LightBulbFloatWin guibg=#fff779')
vim.api.nvim_command('highlight LightBulbVirtualText guibg=#fff779')
