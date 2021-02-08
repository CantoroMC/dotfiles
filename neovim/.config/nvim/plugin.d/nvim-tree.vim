" Section: Features

let g:nvim_tree_width = 26
let g:nvim_tree_ignore =
      \ [ '.git',
      \   'node_modules',
      \   '*.hi',
      \   '*.o',
      \   '*.aux',
      \   '__pycache__',
      \ ]

let g:nvim_tree_auto_open          = 0
let g:nvim_tree_auto_close         = 0
let g:nvim_tree_quit_on_open       = 1
let g:nvim_tree_tab_open           = 0
let g:nvim_tree_follow             = 0
let g:nvim_tree_indent_markers     = 0
let g:nvim_tree_width_allow_resize = 0

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

let g:nvim_tree_bindings = { 
      \   'edit'            : [ '<CR>', 'i' ],
      \   'edit_vsplit'     : '<C-v>',
      \   'edit_split'      : '<C-b>',
      \   'edit_tab'        : '<C-t>',
      \   'close_node'      : [ '<S-CR>', '<BS>' ],
      \   'toggle_ignored'  : 'I',
      \   'toggle_dotfiles' : '.',
      \   'refresh'         : 'R',
      \   'preview'         : '<Tab>',
      \   'cd'              : 'o',
      \   'create'          : 'a',
      \   'remove'          : 'd',
      \   'rename'          : 'r',
      \   'cut'             : 'x',
      \   'copy'            : 'c',
      \   'paste'           : 'p',
      \   'prev_git_item'   : '[g',
      \   'next_git_item'   : ']g',
      \   'dir_up'          : '-',
      \   'close'           : 'q',
      \ }

" Section: Vim Bindings

nnoremap <silent> <F2>   :<C-U>NvimTreeToggle<CR>
nnoremap <silent> <S-F2> :<C-U>NvimTreeFindFile<CR>
nnoremap <silent> <F14>  :<C-U>NvimTreeFindFile<CR>

" vim:fdm=indent
