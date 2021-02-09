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

" -- completion.nvim
" vim.g.completion_confirm_key = ""
" vim.g.completion_matching_strategy_list = {'exact', 'substring', 'fuzzy'}
" vim.g.completion_enable_snippet = 'snippets.nvim'

" -- Decide on length
" vim.g.completion_trigger_keyword_length = 2


" vim.g.completion_chain_complete_list = {
"   default = {
"     { complete_items = { 'lsp' } },
"     { complete_items = { 'buffers' } },
"     { mode = { '<c-p>' } },
"     { mode = { '<c-n>' } }
"   },
"
"" Configure the completion chains
" let g:completion_chain_complete_list = {
" 			\'default' : {
" 			\	'default' : [
" 			\		{'complete_items' : ['lsp', 'snippet']},
" 			\		{'mode' : 'file'}
" 			\	],
" 			\	'comment' : [],
" 			\	'string' : []
" 			\	},
" 			\'vim' : [
" 			\	{'complete_items': ['snippet']},
" 			\	{'mode' : 'cmd'}
" 			\	],
" 			\'c' : [
" 			\	{'complete_items': ['ts']}
" 			\	],
" 			\'python' : [
" 			\	{'complete_items': ['ts']}
" 			\	],
" 			\'lua' : [
" 			\	{'complete_items': ['ts']}
" 			\	],
" 			\} }

let g:completion_matching_strategy_list = [ 'exact', 'substring', 'fuzzy' ]


" Section: Completion-Buffers

let g:completion_word_separator  = '[^a-zA-Z0-9\-_]'
let g:completion_word_min_length = 1
let g:completion_word_ignored_ft = []


" Section: Enable Completion for all buffers

augroup nvim_completion_aug
  autocmd!
  autocmd FileType * lua require'completion'.on_attach()
augroup END
