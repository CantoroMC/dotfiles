" Section: Enable Completion for all buffers

augroup nvim_completion_aug
  autocmd!
  autocmd FileType * lua require'completion'.on_attach()
augroup END

" Section: Completion Menu

let g:completion_enable_auto_popup = 1
inoremap <silent> <expr> <Tab>
      \ pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab>
      \ pumvisible() ? "\<C-p>" : "\<S-Tab>"
imap <silent> <C-P> <Plug>(completion_trigger)

let g:completion_confirm_key       = ""
inoremap <silent> <expr> <Plug>CustomCR
      \ pumvisible() ?
      \   complete_info()['mode'] ==# 'unknown' ?
      \     complete_info()["selected"] != "-1"  ?
      \       "\<Plug>(completion_confirm_completion)" : "\<c-e>\<CR>" :
      \     "\<CR>" : 
      \ "\<CR>"
imap <CR> <Plug>CustomCR

let g:completion_enable_auto_hover      = 1
let g:completion_enable_auto_signature  = 1
let g:completion_enable_auto_paren      = 1
let g:completion_sorting                = "alphabet"
let g:completion_matching_strategy_list = [ 'exact', 'substring', 'fuzzy', 'all' ]
let g:completion_matching_smart_case    = 1
let g:completion_trigger_keyword_length = 1
let g:completion_trigger_on_delete      = 0
let g:completion_timer_cycle            = 80
let g:completion_abbr_length            = 40
let g:completion_menu_length            = 40

" Snippets Support: (UltiSnips, Neosnippet, vim-vsnip, snippets.nvim)
" let g:completion_enable_snippet = 'UltiSnips'


" Chain Completion:
imap <C-X>] <Plug>(completion_next_source)
imap <C-X>[ <Plug>(completion_prev_source)

" Chain List: {{{2
let g:completion_chain_complete_list = {
      \ 'default' : [
      \   {'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux']},
      \   {'mode': ['<c-n>','<c-p>']},
      \ ],
      \ 'c' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'cpp' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'go' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'lua' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'haskell' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'perl' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'python' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'ruby' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ 'vim' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux' ] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \   { 'mode' : 'cmd' },
      \ ],
      \ 'zsh' : [
      \   { 'complete_items': ['lsp', 'snippet', 'path', 'buffers', 'tmux','ts'] },
      \   { 'mode': ['<c-n>','<c-p>'] },
      \ ],
      \ }
" }}}


" Section: Completion-Buffers

let g:completion_word_separator  = '[^a-zA-Z0-9\-_]'
let g:completion_word_min_length = 1
let g:completion_word_ignored_ft = []
