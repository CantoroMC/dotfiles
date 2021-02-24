" Section: Options

let g:airline_detect_modified   = 1
let g:airline_detect_paste      = 1
let g:airline_detect_crypt      = 1
let g:airline_detect_spell      = 0
let g:airline_detect_spelllang  = 0
let g:airline_detect_iminsert   = 1
let g:airline_inactive_collapse = 1
let g:airline_inactive_alt_sep  = 1
let g:airline_powerline_fonts   = 0
let g:airline_symbols_ascii     = 0
let g:airline_exclude_preview   = 1

" define the set of names to be displayed instead of a specific filetypes
" (for section a and b)
let g:airline_filetype_overrides = {
      \ 'fugitive' : ['Fugitive', '%{airline#util#wrap(airline#extensions#branch#get_head(),80)}'],
      \ 'help'     : [ 'Help', '%f' ],
      \ 'NvimTree' : [ ' NvimTree', '' ],
      \ 'startify' : [ 'Startify', '' ],
      \ }



" Section: Appearance

let g:airline#parts#ffenc#skip_expected_string = 'utf-8[unix]'
let g:airline_mode_map = {
      \ '__'     : '-',
      \ 'c'      : 'C',
      \ 'i'      : ' ',
      \ 'ic'     : 'I',
      \ 'ix'     : 'I',
      \ 'n'      : ' ',
      \ 'ni'     : 'N',
      \ 'no'     : 'N',
      \ 'R'      : 'R',
      \ 'Rv'     : 'vR',
      \ 's'      : 'S',
      \ 'S'      : 'S',
      \ ''     : 'S',
      \ 't'      : 'T',
      \ '!'      : 'SH',
      \ 'v'      : 'V',
      \ 'V'      : 'V-L',
      \ ''     : 'V-B',
      \ }
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_left_sep          = ''
let g:airline_right_sep         = ''
let g:airline_symbols.branch    = '⎇ '
let g:airline_symbols.dirty     = '  '
let g:airline_symbols.readonly  = ' '
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.linenr    = ' '
let g:airline_symbols.notexists = '  '

let g:airline#extensions#default#layout = [
      \ [ 'a', 'b', 'c' ],
      \ [ 'error', 'warning', 'x', 'y', 'z' ]
      \ ]



" Section: Builtin extensions

let g:airline_extensions = [
      \   'branch',
      \   'coc',
      \   'fzf',
      \   'fugitiveline',
      \   'hunks',
      \   'nvimlsp',
      \   'po',
      \   'quickfix',
      \   'tabline',
      \   'term',
      \   'undotree',
      \   'vista',
      \   'whitespace',
      \   'wordcount',
      \ ]

let g:airline#extensions#branch#format     = 2
let g:airline#extensions#branch#vcs_checks = [ 'untracked', 'dirty' ]
let g:airline#extensions#hunks#hunk_symbols = [ ' ', '柳 ', ' ' ]

let g:airline#extensions#coc#error_symbol   = ' '
let g:airline#extensions#coc#warning_symbol = ' '

let g:airline#extensions#nvimlsp#error_symbol   = ' '
let g:airline#extensions#nvimlsp#warning_symbol = ' '

let g:airline#extensions#quickfix#quickfix_text = 'QFL'
let g:airline#extensions#quickfix#location_text = 'LCL'

let g:airline#extensions#tabline#formatter       = 'unique_tail_improved'
let g:airline#extensions#tabline#show_splits     = 1
let g:airline#extensions#tabline#show_buffers    = 1
let g:airline#extensions#tabline#show_tab_count  = 0
let g:airline#extensions#tabline#exclude_preview = 1
let g:airline#extensions#tabline#tab_nr_type     = 0
let g:airline#extensions#tabline#buf_label_first = 1
let g:airline#extensions#tabline#buffers_label   = 'B'
let g:airline#extensions#tabline#tabs_label      = 'T'
let g:airline#extensions#tabline#current_first   = 0
let g:airline#extensions#tabline#close_symbol    = ''

let g:airline#extensions#whitespace#mixed_indent_algo = 1
let g:airline#extensions#whitespace#symbol            = ''
let g:airline#extensions#whitespace#checks            = [
      \   'indent',
      \   'trailing',
      \   'mixed-indent-file',
      \   'conflicts'
      \ ]
let g:airline#extensions#whitespace#trailing_format          = 'trail[%s]'
let g:airline#extensions#whitespace#mixed_indent_format      = 'mix-ind[%s]'
let g:airline#extensions#whitespace#mixed_indent_file_format = 'mix-ind-file[%s]'
let g:airline#extensions#whitespace#conflicts_format         = 'confl[%s]'
let g:airline#extensions#whitespace#show_message  = 1
let g:airline#extensions#whitespace#skip_indent_check_ft = {
      \   'markdown': ['trailing']
      \ }

let g:airline#extensions#wordcount#filetypes = [
      \   'asciidoc',
      \   'help',
      \   'mail',
      \   'markdown',
      \   'nroff',
      \   'org',
      \   'plaintex',
      \   'rst',
      \   'tex',
      \   'text',
      \ ]


" Section: Custom 'plugins'
function! Gitsigns() abort
  let signs = get(b:, 'gitsigns_status', '')
  let symbols = [ '柳', ' ', ' ' ]

  let show  = substitute(signs, '\~', '柳', '')
  let show  = substitute(show, '-', ' ', '')
  let show  = substitute(show, '+', ' ', '')

  return show
endfunction

function! AirlineInit()
  let g:airline_section_b = airline#section#create([
        \ 'branch',
        \ " %{Gitsigns()}"
        \ ])
endfunction
autocmd User AirlineAfterInit call AirlineInit()

let g:airline_theme = 'ayu_dark'
