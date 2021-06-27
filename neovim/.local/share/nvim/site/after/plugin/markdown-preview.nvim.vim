let g:mkdp_auto_start         = 0
let g:mkdp_auto_close         = 0
let g:mkdp_refresh_slow       = 1
let g:mkdp_command_for_global = 0
let g:mkdp_open_to_the_world  = 0
let g:mkdp_page_title         = '[Nvim: ${name}]'
let g:mkdp_filetypes          = ['markdown']
let g:mkdp_markdown_css  = expand('~/dotfiles/neovim/.config/nvim/data/mkdp/markdown.css')
let g:mkdp_highlight_css = expand('~/dotfiles/neovim/.config/nvim/data/mkdp/highlight.css')

augroup mkdp_commands
  autocmd!
  autocmd FileType markdown
        \ nnoremap <silent> <buffer> <C-l><C-f>      :<C-U>MarkdownPreview<CR>
  autocmd FileType markdown
        \ nnoremap <silent> <buffer> <C-l><C-f><C-s> :<C-U>MarkdownPreviewStop<CR>
augroup END
