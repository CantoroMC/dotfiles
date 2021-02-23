" Section: Features

let g:nvim_tree_width = 22
let g:nvim_tree_ignore =
      \ [ '.git',
      \   'node_modules',
      \   '*.hi',
      \   '*.o',
      \   '*.aux',
      \   '__pycache__',
      \ ]

let g:nvim_tree_auto_open          = 1
let g:nvim_tree_auto_close         = 0
let g:nvim_tree_quit_on_open       = 1
let g:nvim_tree_tab_open           = 0
let g:nvim_tree_auto_ignore_ft     = [
      \ 'startify'
      \ ]
let g:nvim_tree_follow             = 0
let g:nvim_tree_indent_markers     = 0
let g:nvim_tree_width_allow_resize = 0
let g:nvim_tree_disable_netrw      = 1
let g:nvim_tree_hijack_netrw       = 1

" Section: Appearance
let g:nvim_tree_show_icons =  { 'git' : 1, 'folders' : 1, 'files' : 1 }
let g:nvim_tree_git_hl     = 0
" devicons
let g:nvim_tree_icons = {
      \   'default': ' ',
      \   'symlink': ' ',
      \   'git' : {
      \     'unstaged': '✗',
      \     'staged': '✓',
      \     'unmerged': '',
      \     'renamed': '➜',
      \     'untracked': '★',
      \   },
      \   'folder': {
      \     'default': '',
      \     'open'   : '',
      \     'symlink': '',
      \   },
      \ }

" Section: Nvim-Tree Keymap
lua << EOF
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
EOF



" Section: Vim Bindings

nnoremap <silent> <F2>   :<C-U>NvimTreeToggle<CR>
nnoremap <silent> <S-F2> :<C-U>NvimTreeFindFile<CR>
nnoremap <silent> <F14>  :<C-U>NvimTreeFindFile<CR>

" vim:fdm=indent
