vim.g.indent_blankline_enabled    = 1
vim.g.indent_blankline_char_list  = { '|', '|', '¦', '┆', '┊' }
vim.g.indent_blankline_char_highlight_list = {'Comment', 'Keyword', 'Boolean', 'Special'}
vim.g.indent_blankline_space_char = '·'
vim.g.indent_blankline_space_char_blankline = ' '

vim.g.indent_blankline_show_first_indent_level        = false
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_show_foldtext                  = true


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
  'gitconfig',
  'go',
  'h',
  'help',
  'html',
  'javascript',
  'json',
  'ledger',
  'make',
  'muttrc',
  'neomuttrc',
  'text',
  'markdown',
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
