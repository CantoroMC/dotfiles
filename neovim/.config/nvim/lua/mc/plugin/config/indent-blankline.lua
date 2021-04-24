vim.g.indent_blankline_enabled    = 1
vim.g.indent_blankline_char_list  = { '|', '|', '¦', '┆', '┊' }
vim.g.indent_blankline_space_char = '·'
vim.g.indent_blankline_space_char_blankline = ' '

vim.g.indent_blankline_show_first_indent_level        = false
vim.g.indent_blankline_show_trailing_blankline_indent = false


vim.g.indent_blankline_use_treesitter       = true
vim.g.indent_blankline_show_current_context = true
vim.g.indent_blankline_context_patterns     = {
  'class', 'function', 'method', '^if', '^while', '^for', '^object', '^table',
  'block', 'arguments'
}

vim.g.indent_blankline_filetype_exclude = {
  '',
  'c',
  'cpp',
  'css',
  'diff',
  'go',
  'h',
  'help',
  'html',
  'javascript',
  'json',
  'make',
  'muttrc',
  'neomuttrc',
  'text',
  'GroundHog',
  'NvimTree',
  'packer',
  'startify',
  'undotree'
}
vim.g.indent_blankline_buftype_exclude = {
  'terminal',
  'nofile',
}
