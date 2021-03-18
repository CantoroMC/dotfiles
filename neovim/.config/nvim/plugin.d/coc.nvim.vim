" Section: Extensions
let g:coc_global_extensions = [
      \ 'coc-marketplace',
      \ 'coc-word',
      \ 'coc-syntax',
      \ 'coc-snippets',
      \ 'coc-solargraph',
      \ 'coc-vimlsp',
      \ 'coc-sh',
      \ 'coc-perl',
      \ 'coc-clangd',
      \ 'coc-go',
      \ 'coc-json',
      \ 'coc-tsserver',
      \ 'coc-html',
      \ 'coc-css',
      \ ]
" }}}

" Section: Completion Menu And Snippets Expansion

function! s:check_back_space() abort " {{{2
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" }}}

inoremap <silent> <expr> <Tab>
      \ pumvisible() ?
      \   "\<C-n>" :
      \   (<SID>check_back_space() ? "\<Tab>" : coc#refresh())
inoremap <expr> <S-Tab>
      \ pumvisible() ? "\<C-p>" : "\<S-Tab>"

inoremap <silent> <expr> <Plug>CustomCocCR
      \ pumvisible() ?
      \   coc#_select_confirm() :
      \ "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>\<C-r>=EndwiseDiscretionary()\<CR>"
imap <CR> <Plug>CustomCocCR

let g:coc_snippet_prev = '<c-k>'
let g:coc_snippet_next = '<c-j>'

" Section: Mappings

" Remap <C-f> and <C-b> for scroll float windows/popups.
nnoremap <silent> <nowait> <expr> <C-f>
      \ coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent> <nowait> <expr> <C-b>
      \ coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent> <nowait> <expr> <C-f>
      \ coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<CR>" : "\<Right>"
inoremap <silent> <nowait> <expr> <C-b>
      \ coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<CR>" : "\<Left>"
vnoremap <silent> <nowait> <expr> <C-f>
      \ coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent> <nowait> <expr> <C-b>
      \ coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

augroup coc_map_ft
  autocmd!
  " Diagnostic Navigation
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <LocalLeader>qf  <Plug>(coc-fix-current)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> [g          <Plug>(coc-diagnostic-prev)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> ]g          <Plug>(coc-diagnostic-next)
  " code goto navigation and formatting.
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <silent> <LocalLeader>gd <Plug>(coc-definition)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <silent> <LocalLeader>gD <Plug>(coc-type-definition)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <silent> <LocalLeader>gi <Plug>(coc-implementation)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <silent> <LocalLeader>R <Plug>(coc-references)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ nmap <buffer> <LocalLeader>gq <Plug>(coc-format-selected)
  autocmd FileType
        \ haskell,tex,lua,vim,sh,perl,c,cpp,go,json,typescript,ruby,python
        \ xmap <buffer> <LocalLeader>gq <Plug>(coc-format-selected)
augroup END

nnoremap <silent> <LocalLeader>ca :<C-u>CocAction<CR>
nnoremap <silent> <LocalLeader>dl :<C-u>CocDiagnostic<CR>
nnoremap <silent> <LocalLeader>H :CocHover<CR>
nnoremap <silent> <LocalLeader>co :<C-u>CocList outline<CR>
nnoremap <silent> <LocalLeader>cr :<C-u>CocRestart<CR>
nnoremap <silent> <LocalLeader>cs :<C-u>CocList -I symbols<CR>

command! -nargs=0 CocHover          :call CocActionAsync('doHover')
command! -nargs=0 CocFormat         :call CocAction('format')
