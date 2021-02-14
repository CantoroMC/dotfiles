" Section: Extensions
let g:coc_global_extensions = [
      \ 'coc-marketplace',
      \ 'coc-explorer',
      \ 'coc-word',
      \ 'coc-syntax',
      \ 'coc-omni',
      \ 'coc-snippets',
      \ 'coc-solargraph',
      \ 'coc-vimlsp',
      \ 'coc-sh',
      \ 'coc-perl',
      \ 'coc-clangd',
      \ 'coc-go',
      \ 'coc-json',
      \ 'coc-tsserver',
      \ ]
" }}}

" Section: Completion Menu And Snippets Expansion

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
      \   coc#_select_confirm() :
      \ "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>\<C-r>=EndwiseDiscretionary()\<CR>"
imap <CR> <Plug>CustomCocCR

let g:coc_snippet_prev = '<c-k>'
let g:coc_snippet_next = '<c-j>'

" Section: Mappings

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
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
endif

augroup coc_map_ft
  autocmd!
  " Diagnostic Navigation
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <leader>qf  <Plug>(coc-fix-current)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> [g          <Plug>(coc-diagnostic-prev)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> ]g          <Plug>(coc-diagnostic-next)
  " code goto navigation and formatting.
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <silent> <Leader>gd <Plug>(coc-definition)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <silent> <Leader>gy <Plug>(coc-type-definition)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <silent> <Leader>gi <Plug>(coc-implementation)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <silent> <Leader>go <Plug>(coc-references)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ nmap <buffer> <Leader>gf <Plug>(coc-format-selected)
  autocmd FileType
        \ vim,sh,perl,c,cpp,go,json,typescript,ruby,haskell,lhaskell,tex,bib,plaintex,context,python
        \ xmap <buffer> <Leader>gf <Plug>(coc-format-selected)
augroup END

nnoremap <silent> <Leader>cA :<C-u>CocAction<CR>
nnoremap <silent> <Leader>cD :<C-u>CocDiagnostic<CR>
nnoremap <silent> <leader>cH :CocHover<CR>
nnoremap <silent> <Leader>cO :<C-u>CocList outline<CR>
nnoremap <silent> <Leader>cR :<C-u>CocRestart<CR>
nnoremap <silent> <Leader>cS :<C-u>CocList -I symbols<CR>

command! -nargs=0 CocHover          :call CocActionAsync('doHover')
command! -nargs=0 CocFormat         :call CocAction('format')



" Coc Explorer:

nnoremap <silent> <F2> :CocCommand explorer<CR>
nmap <Leader>er :call CocAction('runCommand', 'explorer.doAction', 'closest', ['reveal:0'], [['relative', 0, 'file']])<CR>

let g:coc_explorer_global_presets = {
\   '.vim': {
\     'root-uri': '~/.local/share/nvim',
\   },
\   'cocConfig': {
\      'root-uri': '~/.config/coc',
\   },
\   'tab': {
\     'position': 'tab',
\     'quit-on-open': v:true,
\   },
\   'floating': {
\     'position': 'floating',
\     'open-action-strategy': 'sourceWindow',
\   },
\   'floatingTop': {
\     'position': 'floating',
\     'floating-position': 'center-top',
\     'open-action-strategy': 'sourceWindow',
\   },
\   'floatingLeftside': {
\     'position': 'floating',
\     'floating-position': 'left-center',
\     'floating-width': 50,
\     'open-action-strategy': 'sourceWindow',
\   },
\   'floatingRightside': {
\     'position': 'floating',
\     'floating-position': 'right-center',
\     'floating-width': 50,
\     'open-action-strategy': 'sourceWindow',
\   },
\   'simplify': {
\     'file-child-template': '[selection | clip | 1] [indent][icon | 1] [filename omitCenter 1]'
\   },
\   'buffer': {
\     'sources': [{'name': 'buffer', 'expand': v:true}]
\   },
\ }

" open-action-strategy: select, vsplit, split, tab, previousBuffer,
" previousWindow sourceWindow

" Use preset argument to open it
nmap <space>ed :CocCommand explorer --preset .vim<CR>
nmap <space>ef :CocCommand explorer --preset floating<CR>
nmap <space>ec :CocCommand explorer --preset cocConfig<CR>
nmap <space>eb :CocCommand explorer --preset buffer<CR>

" List all presets
nmap <space>el :CocList explPresets
