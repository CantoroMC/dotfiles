" autoload/fzf_hoogle.vim

" Section: Auto Loading Guards

if exists('g:autoloaded_fzf_hoogle')
  finish
endif
let g:autoloaded_fzf_hoogle = 1

if !executable('jq')
  echomsg '[fzf_hoogle] `jq` is not installed. Plugin disabled.'
  finish
endif

let s:hoogle_path = get(g:, 'hoogle_path', 'hoogle')
if !executable(s:hoogle_path)
  echomsg '[fzf_hoogle] `hoogle` is not installed. Plugin disabled.'
  finish
endif

command! -nargs=* -bang Hoogle call haskell#fzf_hoogle#run(<q-args>, <bang>0)
