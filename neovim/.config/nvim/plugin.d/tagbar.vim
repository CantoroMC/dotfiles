let g:tagbar_autoclose            = 0
let g:tagbar_autofocus            = 0
let g:tagbar_autoshowtag          = 1
let g:tagbar_case_insensitive     = 1
let g:tagbar_compact              = 1
let g:tagbar_foldlevel            = 2
let g:tagbar_previewwin_pos       = ''
let g:tagbar_show_balloon         = 1
let g:tagbar_show_data_type       = 1
let g:tagbar_show_linenumbers     = 0
let g:tagbar_show_tag_linenumbers = 2
let g:tagbar_show_visibility      = 1
let g:tagbar_width                = 25
let g:tagbar_visibility_symbols   = {
      \   'public'   : '+',
      \   'protected': '#',
      \   'private'  : '-',
      \ }
let g:tagbar_scopestrs = {
      \   'class':          "\uf0e8",
      \   'const':          "\uf8ff",
      \   'constant':       "\uf8ff",
      \   'enum':           "\uf702",
      \   'field':          "\uf30b",
      \   'func':           "\uf794",
      \   'function':       "\uf794",
      \   'getter':         "\ufab6",
      \   'implementation': "\uf776",
      \   'interface':      "\uf7fe",
      \   'map':            "\ufb44",
      \   'member':         "\uf02b",
      \   'method':         "\uf6a6",
      \   'setter':         "\uf7a9",
      \   'variable':       "\uf71b",
      \ }

let g:no_status_line = 1

nnoremap <silent> <C-X><C-T> :<C-u>TagbarToggle<CR>
