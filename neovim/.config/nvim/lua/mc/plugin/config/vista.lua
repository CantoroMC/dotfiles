vim.g.vista_sidebar_position     = 'vertical botright'
vim.g.vista_sidebar_width        = 25
vim.g.bista_echo_cursor          = 0
vim.g.vista_echo_cursor_strategy = 'floating_win'
vim.g.vista_close_on_fzf_select  = 1
vim.g.vista_stay_on_open         = 0
vim.g.vista_default_executive    = 'ctags'
vim.g.vista_keep_fzf_colors      = 1
vim.g.vista_disable_statusline   = 1
vim.g.vista_executive_for = {
  ["c"]          = 'coc',
  ["cpp"]        = 'coc',
  ["perl"]       = 'coc',
  ["json"]       = 'coc',
  ["python"]     = 'coc',
  ["ruby"]       = 'coc',
  ["typescript"] = 'coc',
  ["sh"]         = 'coc',
  ["zsh"]        = 'coc',
  ["haskell"]    = 'nvim_lsp',
  ["lua"]        = 'nvim_lsp',
  ["vim"]        = 'nvim_lsp',
}

vim.api.nvim_exec([[
  let g:vista#renderer#enable_icon = 1
]], false)


vim.api.nvim_set_keymap('n', '<C-x><C-v>',
  '<Cmd>Vista!!<CR>', { noremap = true, silent = true })
