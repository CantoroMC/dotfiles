function! StartifyEntryFormat() abort " {{{1
  return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction
" }}}

function! s:gitModified() abort " {{{1
  let files = systemlist('git ls-files -m 2>/dev/null')
  return map(files, "{'line': v:val, 'path': v:val}")
endfunction
" }}}

function! s:gitUntracked() abort " {{{1
  let files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(files, "{'line': v:val, 'path': v:val}")
endfunction
" }}}

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
      \   'header': ['     git modified']
      \ },
      \ {
      \   'type': function('s:gitUntracked'),
      \   'header': ['     git untracked']
      \ },
      \ ]

let g:startify_bookmarks = [
      \ '~/dotfiles/nvim/.config/nvim/init.vim',
      \ '~/dotfiles/nvim/.local/share/nvim/wiki/index.md',
      \ '~/Documents/programming/TeX/matriHX/matriHX.tex',
      \]

let g:startify_session_dir         = '~/.cache/nvim/sessions'
let g:startify_disable_at_vimenter = 0
let g:startify_files_number        = 10
let g:startify_update_oldfiles     = 1
let g:startify_change_to_dir       = 0
let g:startify_change_to_vcs_root  = 1
let g:startify_fortune_use_unicode = 0
let g:startify_custom_header       =
      \ 'startify#center(startify#fortune#cowsay())'
