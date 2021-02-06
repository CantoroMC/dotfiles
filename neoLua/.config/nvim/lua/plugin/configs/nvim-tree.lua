-- TODO: check if they decide to reactivate netrw
Set_var {
  nvim_tree_side                 = { 'g', 'left' },
  nvim_tree_width                = { 'g', 28 },
  nvim_tree_ignore               = { 'g', { '.git',
                                            'node_modules',
                                            '*.hi',
                                            '*.o',
                                            '*.aux',
                                            '__pycache__',
                                          } },
  nvim_tree_auto_open            = { 'g', 1 },
  nvim_tree_auto_close           = { 'g', 1 },
  nvim_tree_quit_on_open         = { 'g', 0 },
  nvim_tree_follow               = { 'g', 0 },
  nvim_tree_indent_markers       = { 'g', 1 },
  nvim_tree_hide_dotfiles        = { 'g', 0 },
  nvim_tree_git_hl               = { 'g', 1 },
  nvim_tree_root_folder_modifier = { 'g', ':~' },
  nvim_tree_tab_open             = { 'g', 0 },
  nvim_tree_width_allow_resize   = { 'g', 0 },
  nvim_tree_show_icons           = { 'g', { git = 1,
                                            folders = 1,
                                            files   = 1
                                          }
                                   },
  nvim_tree_bindings             = { 'g', { edit            = { '<CR>', 'i' },
                                            edit_vsplit     = '<C-v>',
                                            edit_split      = '<C-b>',
                                            edit_tab        = '<C-t>',
                                            close_node      = { '<S-CR>', '<BS>' },
                                            toggle_ignored  = 'I',
                                            toggle_dotfiles = '.',
                                            refresh         = 'R',
                                            preview         = '<Tab>',
                                            cd              = 'o',
                                            create          = 'a',
                                            remove          = 'd',
                                            rename          = 'r',
                                            cut             = 'x',
                                            copy            = 'c',
                                            paste           = 'p',
                                            prev_git_item   = '[g',
                                            next_git_item   = ']g',
                                            dir_up          = '-',
                                            close           = 'q',
                                            }
                                   },
  -- devicons
  nvim_tree_icons                = { 'g', { default = '',
                                            symlink = '',
                                            git = { unstaged  = "✗",
                                                    staged    = "✓",
                                                    unmerged  = "",
                                                    renamed   = "➜",
                                                    untracked = "★"
                                                  },
                                            folder = { default = "",
                                                       open    = "",
                                                       symlink = "",
                                                     },
                                          },
                                   },
}
  -- NOTE: Also a lot of highlighting groups are defined and can be customized
  -- as you like.

Map( 'n', '<F2>', ':<C-U>NvimTreeToggle<CR>', { noremap = true, silent = true } )
Map( 'n', '<F14>', ':<C-U>NvimTreeFindFile<CR>', { noremap = true, silent = true } )
Map( 'n', '<S-F2>', ':<C-U>NvimTreeFindFile<CR>', { noremap = true, silent = true } )
