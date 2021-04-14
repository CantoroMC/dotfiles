vim.g.tagbar_autoclose            = 0
vim.g.tagbar_autofocus            = 0
vim.g.tagbar_autoshowtag          = 1
vim.g.tagbar_case_insensitive     = 1
vim.g.tagbar_compact              = 1
vim.g.tagbar_foldlevel            = 2
vim.g.tagbar_previewwin_pos       = ''
vim.g.tagbar_show_balloon         = 1
vim.g.tagbar_show_data_type       = 1
vim.g.tagbar_show_linenumbers     = 0
vim.g.tagbar_show_tag_linenumbers = 2
vim.g.tagbar_show_visibility      = 1
vim.g.tagbar_width                = 25
vim.g.no_status_line              = 1
vim.g.tagbar_visibility_symbols   = {
  ['public']    = '+',
  ['protected'] = '#',
  ['private']   = '-',
}
vim.g.tagbar_scopestrs = {
  ["class"]          = ' ',
  ["const"]          = ' ',
  ["constant"]       = ' ',
  ["enum"]           = '練',
  ["field"]          = ' ',
  ["func"]           = '',
  ["function"]       = '',
  ["getter"]         = '鹿',
  ["implementation"] = '',
  ["interface"]      = '蘒',
  ["map"]            = '',
  ["member"]         = ' ',
  ["method"]         = '',
  ["setter"]         = '鹿',
  ["variable"]       = '',
}

vim.api.nvim_set_keymap('n', '<C-x><C-t>',
  '<Cmd>TagbarToggle<CR>', { noremap = true, silent = true })
