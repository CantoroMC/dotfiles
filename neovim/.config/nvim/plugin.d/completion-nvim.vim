" Section: Completion Menu And Snippets Expansion:

inoremap <silent> <expr> <Tab>
      \ pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab>
      \ pumvisible() ? "\<C-p>" : "\<S-Tab>"
 
let g:completion_enable_auto_popup = 1
imap <silent> <C-n> <Plug>(completion_trigger)
" let g:completion_trigger_character = [ "\<C-n>", "\<C-p>" ]

" possible value: 'UltiSnips', 'Neosnippet', 'vim-vsnip', 'snippets.nvim'
" let g:completion_enable_snippet = 'UltiSnips'

" let g:completion_confirm_key = ""
" imap <expr> <CR>  pumvisible() ? complete_info()["selected"] != "-1" ?
"       \ "\<Plug>(completion_confirm_completion)"  :
"       \ "\<c-e>\<CR>" : "\<CR>"

let g:completion_enable_auto_hover = 1
let g:completion_enable_auto_signature = 1
let g:completion_enable_auto_paren = 1

let g:completion_chain_complete_list = {
      \  'default' : [
      \    {'complete_items': ['lsp', 'snippet', 'path']},
      \    {'mode': '<c-p>'},
      \    {'mode': '<c-n>'}
      \  ]
      \ }

let g:completion_matching_strategy_list = [ 'exact', 'substring', 'fuzzy' ]
