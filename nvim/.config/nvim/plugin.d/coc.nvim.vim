" Extensions: {{{1
let g:coc_global_extensions = [
      \ 'coc-marketplace',
      \ 'coc-word',
      \ 'coc-highlight',
      \ 'coc-syntax',
      \ 'coc-omni',
      \ 'coc-snippets',
      \ 'coc-solargraph',
      \ 'coc-texlab',
      \ 'coc-vimlsp',
      \ 'coc-sh',
      \ 'coc-python',
      \ 'coc-perl',
      \ 'coc-clangd',
      \ 'coc-go',
      \ 'coc-json',
      \ 'coc-tsserver',
      \ ]
" }}}

" Completion Menu And Snippets Expansion: {{{1

function! s:check_back_space() abort " {{{2
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" }}}

inoremap <silent> <expr> <Tab>
      \ pumvisible() ? "\<C-n>" : <SID>check_back_space() ?
      \ "\<Tab>" : coc#refresh()
inoremap <expr> <S-Tab>
      \ pumvisible() ? "\<C-p>" : "\<S-Tab>"

inoremap <silent> <expr> <Plug>CustomCocCR
      \ pumvisible() ?
      \ coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
imap <CR> <Plug>CustomCocCR

let g:coc_snippet_prev = '<c-k>'
let g:coc_snippet_next = '<c-j>'
" }}}

" Mappings: {{{1
" Diagnostic Navigation
nmap <leader>qf  <Plug>(coc-fix-current)
nmap [g          <Plug>(coc-diagnostic-prev)
nmap ]g          <Plug>(coc-diagnostic-next)
" code goto navigation and formatting.
nmap <silent> <Leader>gd <Plug>(coc-definition)
nmap <silent> <Leader>gy <Plug>(coc-type-definition)
nmap <silent> <Leader>gi <Plug>(coc-implementation)
nmap <silent> <Leader>go <Plug>(coc-references)
nmap          <Leader>gf <Plug>(coc-format-selected)
xmap          <Leader>gf <Plug>(coc-format-selected)

nnoremap <silent> <Leader>cA :<C-u>CocAction<CR>
nnoremap <silent> <Leader>cD :<C-u>CocDiagnostic<CR>
nnoremap <silent> <Leader>cR :<C-u>CocRestart<CR>
nnoremap <silent> <Leader>cS :<C-u>CocList snippets<CR>
nnoremap <silent> <Leader>cO :<C-u>CocList outline<CR>
nnoremap <silent> <leader>cH :call CocActionAsync('doHover')<CR>

command! CocPickHigh call CocAction('colorPresentation')
augroup coc_highlight
  autocmd CursorHold * silent call CocActionAsync('highlight')
augroup END
" }}}
