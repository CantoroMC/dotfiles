let g:vista_sidebar_position     = 'vertical botright'
let g:vista_sidebar_width        = 25
let g:bista_echo_cursor          = 0
let g:vista_echo_cursor_strategy = 'floating_win'
let g:vista_close_on_fzf_select  = 1
let g:vista_stay_on_open         = 0
let g:vista_default_executive    = 'ctags'
let g:vista_keep_fzf_colors      = 1
let g:vista_disable_statusline   = 1
let g:vista#renderer#enable_icon = 1

let g:vista_executive_for = {
      \ 'c'         : 'coc',
      \ 'cpp'       : 'coc',
      \ 'perl'      : 'coc',
      \ 'json'      : 'coc',
      \ 'python'    : 'coc',
      \ 'ruby'      : 'coc',
      \ 'typescript': 'coc',
      \ 'sh'        : 'coc',
      \ 'zsh'       : 'coc',
      \ 'haskell'   : 'nvim_lsp',
      \ 'lua'       : 'nvim_lsp',
      \ 'vim'       : 'nvim_lsp',
      \ }

nnoremap <silent> <C-x><C-v> :<C-u>Vista!!<CR>
