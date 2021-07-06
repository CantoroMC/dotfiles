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
vim.g.nvim_tree_disable_window_picker = 0
vim.g.nvim_tree_hijack_cursor = 1
vim.g.nvim_tree_icon_padding = ' '
vim.g.nvim_tree_update_cwd = 0
vim.g.nvim_tree_window_picker_exclude = {
  ["filetype"] = { 'packer', 'qf' },
  ["buftype"] =  { 'terminal' },
}
vim.g.nvim_tree_special_files = {
  'README.md', 'Makefile', 'MAKEFILE'
}

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
  ["default"] = '',
  ["symlink"] = '',
  ["git"]     = {
    ["unstaged"]  = '✗',
    ["staged"]    = '✓',
    ["unmerged"]  = '',
    ["renamed"]   = '➜',
    ["untracked"] = '★',
    ["deleted"]   = "",
    ["ignored"]   = "◌"
  },
  ["folder"] = {
    ["arrow_open" ]   = "",
    ["arrow_closed" ] = "",
    ["default" ]      = "",
    ["open" ]         = "",
    ["empty" ]        = "",
    ["empty_open" ]   = "",
    ["symlink" ]      = "",
    ["symlink_open" ] = "",
  },
  ["lsp"] = {
    ["hint"]    = "",
    ["info"]    = "",
    ["warning"] = "",
    ["error"]   = "",
  }
}


-- KEYMAP:
vim.g.nvim_tree_disable_default_keybindings = 1
function _G.NvimTreeOSOpen()
  local lib = require "nvim-tree.lib"
  local node = lib.get_node_at_cursor()
  if node then
    vim.fn.jobstart("xdg-open '" .. node.absolute_path .. "' &", {detach = true})
  end
end

local tree_cb = require'nvim-tree.config'.nvim_tree_callback
vim.g.nvim_tree_bindings = {
  { key = "<CR>",             cb = tree_cb("edit") },
  { key = "<C-v>",            cb = tree_cb("vsplit") },
  { key = "<C-b>",            cb = tree_cb("split") },
  { key = "<C-t>",            cb = tree_cb("tabnew") },
  { key = "<Tab>",            cb = tree_cb("preview") },

  { key = "K",                cb = tree_cb("first_sibling") },
  { key = "J",                cb = tree_cb("last_sibling") },
  { key = "<",                cb = tree_cb("prev_sibling") },
  { key = ">",                cb = tree_cb("next_sibling") },

  { key = "I",                cb = tree_cb("toggle_ignored") },
  { key = ".",                cb = tree_cb("toggle_dotfiles") },

  { key = "R",                cb = tree_cb("refresh") },
  { key = "a",                cb = tree_cb("create") },
  { key = "d",                cb = tree_cb("remove") },
  { key = "r",                cb = tree_cb("rename") },
  { key = "<C->",             cb = tree_cb("full_rename") },

  { key = "x",                cb = tree_cb("cut") },
  { key = "c",                cb = tree_cb("copy") },
  { key = "p",                cb = tree_cb("paste") },
  { key = "y",                cb = tree_cb("copy_name") },
  { key = "Y",                cb = tree_cb("copy_path") },
  { key = "gy",               cb = tree_cb("copy_absolute_path") },

  { key = "[g",               cb = tree_cb("prev_git_item") },
  { key = "]g",               cb = tree_cb("next_git_item") },
  { key = "<C-p>",            cb = tree_cb("dir_up") },
  { key = "o",                cb = tree_cb("cd") },
  { key = "O",                cb = ":lua NvimTreeOSOpen()<cr>" },
  { key = {"<BS>", "<S-CR>"}, cb = tree_cb("close_node") },
  { key = "P",                cb = tree_cb("parent_node") },
  { key = "q",                cb = tree_cb("close") },
  { key = "g?",               cb = tree_cb("toggle_help") },
}

-- BINDINGS:
vim.api.nvim_set_keymap('n', '<C-c>f',
  '<Cmd>NvimTreeToggle<CR>',   { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-c><C-f>',
  '<Cmd>NvimTreeFindFile<CR>', { noremap = true, silent = true })
