-- FEATURES:
vim.g.nvim_tree_side               = 'left'
vim.g.nvim_tree_width              = 22
vim.g.nvim_tree_auto_open          = 1
vim.g.nvim_tree_auto_close         = 0
vim.g.nvim_tree_quit_on_open       = 1
vim.g.nvim_tree_tab_open           = 0
vim.g.nvim_tree_follow             = 1
vim.g.nvim_tree_indent_markers     = 1
vim.g.nvim_tree_git_hl             = 0
vim.g.nvim_tree_width_allow_resize = 0
vim.g.nvim_tree_disable_netrw      = 1
vim.g.nvim_tree_hijack_netrw       = 1
vim.g.nvim_tree_add_trailing       = 1
vim.g.nvim_tree_group_empty        = 1
vim.g.nvim_tree_lsp_diagnostics    = 1

vim.g.nvim_tree_ignore = {
  '.git',
  'node_modules',
  '*.hi',
  '*.o',
  '*.aux',
  '__pycache__',
}
vim.g.nvim_tree_auto_ignore_ft = { 'startify' }


-- ICONS:
vim.g.nvim_tree_show_icons =  {
  ["git"]     = 1,
  ["folders"] = 1,
  ["files"]   = 1
}
vim.g.nvim_tree_icons = {
  ["default"] = ' ',
  ["symlink"] = ' ',
  ["git"]     = {
    ["unstaged"]  = '✗',
    ["staged"]    = '✓',
    ["unmerged"]  = '',
    ["renamed"]   = '➜',
    ["untracked"] = '★',
  },
  ["folder"] = {
    ["default"] = ' ',
    ["open"   ] = ' ',
    ["symlink"] = ' ',
  },
}


-- KEYMAP:
local function get_lua_cb(cb_name)
  return string.format(":lua require'nvim-tree'.on_keypress('%s')<CR>", cb_name)
end

vim.g.nvim_tree_bindings = {
  ["<CR>"]   = get_lua_cb("edit"),
  ["<C-v>"]  = get_lua_cb("vsplit"),
  ["<C-b>"]  = get_lua_cb("split"),
  ["<C-t>"]  = get_lua_cb("tabnew"),
  ["<BS>"]   = get_lua_cb("close_node"),
  ["<S-CR>"] = get_lua_cb("close_node"),
  ["I"]      = get_lua_cb("toggle_ignored"),
  ["."]      = get_lua_cb("toggle_dotfiles"),
  ["R"]      = get_lua_cb("refresh"),
  ["<Tab>"]  = get_lua_cb("preview"),
  ["o"]      = get_lua_cb("cd"),
  ["a"]      = get_lua_cb("create"),
  ["d"]      = get_lua_cb("remove"),
  ["r"]      = get_lua_cb("rename"),
  ["x"]      = get_lua_cb("cut"),
  ["c"]      = get_lua_cb("copy"),
  ["p"]      = get_lua_cb("paste"),
  ["[g"]     = get_lua_cb("prev_git_item"),
  ["]g"]     = get_lua_cb("next_git_item"),
  ["<C-P>"]  = get_lua_cb("dir_up"),
  ["q"]      = get_lua_cb("close"),
}


-- BINDINGS:
vim.api.nvim_set_keymap('n', '<C-c>f',
  '<Cmd>NvimTreeToggle<CR>',   { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-c><C-f>',
  '<Cmd>NvimTreeFindFile<CR>', { noremap = true, silent = true })
