if exists('b:did_ftplugin')
  finish
endif
source $VIMRUNTIME/ftplugin/go.vim

nnoremap <buffer> <silent> <S-F5> :GoRun<CR>
nnoremap <buffer> <silent> <F17>  :GoRun<CR>
nnoremap <buffer>          <F5>   :execute '! '.fnameescape(expand('%:p:r'))
nnoremap <buffer> <silent> <F8>   :GoBuild<CR>
nnoremap <buffer> <silent> [go    :GoFmt<CR>

command! GoFmt   :execute '!gofmt -w '.fnameescape(expand('%:p'))
command! GoRun   :execute '!go run '.fnameescape(expand('%:p'))
command! GoBuild :execute '!go build '.fnameescape(expand('%:p'))
