if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal comments=s1fl:{-,mb:-,ex:-},:--
setlocal commentstring=--\ %s
setlocal formatoptions=jcrqlnB

command! -buffer HsFormat
      \ :execute '!brittany --write-mode=inplace '.fnameescape(expand('%:p'))
nnoremap <buffer> <silent> <C-c><C-f> :HsFormat<CR>

let b:undo_ftplugin = "setl com< cms< fo<"
