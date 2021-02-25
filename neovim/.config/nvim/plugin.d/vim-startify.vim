function! StartifyEntryFormat() abort
  return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction

" Session:
let g:startify_session_dir         = stdpath('data').'/sessions'
let g:startify_session_autoload    = 1
let g:startify_session_before_save = [
      \ 'echomsg "Cleaning up before saving.."',
      \ 'silent! NvimTreeClose'
      \ ]
let g:startify_session_persistence    = 1
let g:startify_session_delete_buffers = 0
let g:startify_session_savevars       = [
      \ 'g:startify_session_savevars',
      \ 'g:startify_session_savecmds',
      \ ]
let g:startify_session_number         = 10
let g:startify_session_sort           = 1

" Intro:

" Entries:

function! s:gitModified() abort
  let files = systemlist('git ls-files -m 2>/dev/null')
  return map(files, "{'line': v:val, 'path': v:val}")
endfunction

function! s:gitUntracked() abort
  let files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(files, "{'line': v:val, 'path': v:val}")
endfunction

let g:startify_lists = [
      \ {
      \   'type': 'dir',
      \   'header': [
      \     '     Current Directory: '.substitute(getcwd(), $HOME, '~', 'g')]
      \ },
      \ {
      \   'type': 'files',
      \   'header': ['     Recents:']
      \ },
      \ {
      \   'type': 'sessions',
      \   'header': ['     Sessions']
      \ },
      \ {
      \   'type': 'bookmarks',
      \   'header': ['     Bookmarks']
      \ },
      \ {
      \   'type': function('s:gitModified'),
      \   'header': ['   柳  git modified']
      \ },
      \ {
      \   'type': function('s:gitUntracked'),
      \   'header': ['      git untracked']
      \ },
      \ {
      \   'type': 'commands',
      \   'header': ['     Commands']},
      \ ]
" }}}
" Book Marks: {{{1
let g:startify_bookmarks = [
      \ '~/.config/nvim/init.lua',
      \ '~/.config/zsh/.zshrc',
      \ '~/Documents/programming/TeX/projects/matriHX/matriHX.tex',
      \]
" }}}
" Commands: {{{1
let g:startify_commands = [
    \ ['Git', ':Git'],
    \ {'s': ['Load Last Session', ':SLoad!'] },
    \ {'p': ['Vim Startup Time', ':StartupTime']},
    \ ]
" }}}

let g:startify_disable_at_vimenter = 0
let g:startify_files_number        = 10
let g:startify_change_to_dir       = 0
let g:startify_change_to_vcs_root  = 0
let g:startify_padding_left        = 3
let g:startify_enable_special      = 1
let g:startify_update_oldfiles     = 1
let g:startify_fortune_use_unicode = 0
let g:startify_custom_header       =
      \ startify#fortune#cowsay('', '═','║','╔','╗','╝','╚')
let g:startify_relative_path = 1
let g:startify_use_env = 1
