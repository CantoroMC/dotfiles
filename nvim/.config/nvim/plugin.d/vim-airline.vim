let g:airline_powerline_fonts  = 0
let g:airline_detect_spell     = 0
let g:airline_detect_spelllang = 0
let g:airline_detect_iminsert  = 1
let g:airline_left_sep         = ''
let g:airline_right_sep        = ''

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.branch = ''
let g:airline_symbols.linenr = '⭡'
let g:airline_symbols.dirty  = ' '
let g:airline_symbols.maxlinenr = ''

let g:airline#parts#ffenc#skip_expected_string = 'utf-8[unix]'

" Extensions: {{{2

let g:airline#extensions#tabline#enabled           = 1
let g:airline#extensions#tabline#formatter         = 'unique_tail_improved'
let g:airline#extensions#tabline#left_alt_sep      = '|'
let g:airline#extensions#tabline#left_sep          = ' '
let g:airline#extensions#tabline#show_buffers      = 1
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_splits       = 1
let g:airline#extensions#tabline#show_tab_count    = 0
let g:airline#extensions#tabline#tab_nr_type       = 2

let g:airline#extensions#branch#enabled           = 1
let g:airline#extensions#branch#format            = 2
let g:airline#extensions#coc#enabled              = 1
let g:airline#extensions#fzf#enabled              = 1
let g:airline#extensions#fugitiveline#enabled     = 1
let g:airline#extensions#nerdtree_statusline      = 1
let g:airline#extensions#neomake#enabled          = 1
let g:airline#extensions#quickfix#quickfix_text   = 'Quickfix'
let g:airline#extensions#quickfix#location_text   = 'Location'
let g:airline#extensions#tagbar#enabled           = 0
let g:airline#extensions#term#enabled             = 1
let g:airline#extensions#whitespace#enabled       = 1
let g:airline#extensions#whitespace#checks        =
      \  ['indent', 'trailing', 'mixed-indent-file', 'conflicts' ]
let g:airline#extensions#whitespace#show_message  = 1
let g:airline#extensions#whitespace#skip_indent_check_ft = {
      \ 'markdown': ['trailing']}
let g:airline#extensions#wordcount#enabled        = 1
let g:airline#extensions#wordcount#filetypes      = ['all']

" }}}
