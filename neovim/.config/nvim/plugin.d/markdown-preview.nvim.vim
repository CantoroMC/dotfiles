let g:mkdp_auto_start         = 0
let g:mkdp_auto_close         = 0
let g:mkdp_refresh_slow       = 1
let g:mkdp_command_for_global = 0
let g:mkdp_open_to_the_world  = 0
let g:mkdp_filetypes          = ['markdown']

augroup mkdp_commands
  autocmd!
  autocmd FileType markdown
        \ nnoremap <silent> <buffer> <F9>   :<C-U>MarkdownPreview<CR>
  autocmd FileType markdown
        \ nnoremap <silent> <buffer> <S-F9> :<C-U>MarkdownPreviewStop<CR>
  autocmd FileType markdown
        \ nnoremap <silent> <buffer> <F21>  :<C-U>MarkdownPreviewStop<CR>
augroup END
