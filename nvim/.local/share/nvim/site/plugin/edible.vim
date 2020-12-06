 " Vim Plugin: {{{1

" File: edible
" Author: Marco Cantoro
" Description: text editing utilities
" Last Modified: agosto 28, 2020

" }}}

" Plugin Guards: {{{1

if exists('g_loaded_edible')
  finish
endif
let g:loaded_edible = 1

" }}}

" Edible: {{{1
"                                                                            "
"                             /$$ /$$ /$$       /$$                          "
"                            | $$|__/| $$      | $$                          "
"              /$$$$$$   /$$$$$$$ /$$| $$$$$$$ | $$  /$$$$$$                 "
"             /$$__  $$ /$$__  $$| $$| $$__  $$| $$ /$$__  $$                "
"            | $$$$$$$$| $$  | $$| $$| $$  \ $$| $$| $$$$$$$$                "
"            | $$_____/| $$  | $$| $$| $$  | $$| $$| $$_____/                "
"            |  $$$$$$$|  $$$$$$$| $$| $$$$$$$/| $$|  $$$$$$$                "
"             \_______/ \_______/|__/|_______/ |__/ \_______/                "
"                                                                            "
" }}}

" Reimplemented Control E And Y: {{{1

inoremap <silent> <C-Y> <C-R><C-R>=edible#look_up_or_down(0)<CR>
inoremap <silent> <C-E> <C-R><C-R>=edible#look_up_or_down(1)<CR>

" }}}

" Scratch Buffer: {{{1

" Global Variables: {{{2
let s:options = [
      \ [ 'name', '__Scratch__'],
      \ [ 'pos' ,      'bottom'],
      \ [ 'size',          0.25],
      \ ]

for [opt, val] in s:options
  if !exists('g:edible_scratch_'.opt)
    execute 'let g:edible_scratch_'.opt.' = '.string(val)
  endif
endfor
" }}}

command! Scratch :call edible#toggleScratch()
nnoremap <silent> <Plug>EdibleScratch
      \ :<C-U>Scratch<CR>
if !hasmapto('<Plug>EdibleScratch', 'n') && maparg('<Leader>S', 'n') ==# ''
  nmap <Leader>S <Plug>EdibleScratch
endif

" }}}

" Strip Trailing White Spaces: {{{1

" Global Variables: {{{2
if !exists('g:edible_ignored_filetypes')
  let g:edible_ignored_filetypes = [
        \ 'markdown',
        \ 'dosini'
        \ ]
endif
" }}}

command! StripTrailingSpaces :call edible#strip_trailing_spaces()
nnoremap <silent> <Plug>EdibleStripTrailingSpaces
      \ :<C-U>StripTrailingSpaces<CR>
if !hasmapto('<Plug>EdibleStripTrailingSpaces', 'n') &&
      \ maparg('<LocalLeader>fw', 'n') ==# ''
  nmap <LocalLeader>fw <Plug>EdibleStripTrailingSpaces
endif
cabbrev STS StripTrailingSpaces

" " TODO: It consume few sec for very long files, it may be bad
" " TODO: VimLeave, BufWinEnter don t do for not modifiable and read only files
augroup edible_strip_trailing_spaces
  autocmd!
  autocmd BufWritePre * call edible#strip_trailing_spaces()
augroup END

" }}}

" Strip Blank Lines: {{{1
command! StripBlankLines :call edible#strip_blank_lines()
" }}}

" Re Indent Whole Buffer: {{{1

command! ReindentWhole :execute 'normal! mzgg=G`z'

nnoremap <silent> <Plug>EdibleReindentWhole :<C-U>ReindentWhole<CR>

if !hasmapto('<Plug>EdibleReindentWhole', 'n') &&
      \ maparg('<Leader>=', 'n') ==# ''
  nmap <Leader>= <Plug>EdibleReindentWhole
endif

" }}}

" Title Case: {{{1

nnoremap <silent> <Plug>EdibleTitlecase
      \ :<C-U>set operatorfunc=edible#titlecase<CR>g@
nnoremap <silent> <Plug>EdibleTitlecaseLine
      \ :<C-U>set operatorfunc=edible#titlecase <Bar> execute 'normal! '
      \ .v:count1.'g@_'<CR>
xnoremap <silent> <Plug>EdibleTitlecase
      \ :<C-U>call edible#titlecase(
      \   visualmode(), visualmode() ==# 'V' ? 1 : 0)<CR>

if !hasmapto('<Plug>EdibleTitlecase', 'n') && maparg('gz','n') ==# ''
  nmap gz <Plug>EdibleTitlecase
endif
if !hasmapto('<Plug>EdibleTitlecaseLine', 'n') && maparg('gzz','n') ==# ''
  nmap gzz <Plug>EdibleTitlecaseLine
endif
if !hasmapto('<Plug>EdibleTitlecase', 'x') && maparg('gz','x') ==# ''
  xmap gz <Plug>EdibleTitlecase
endif

" }}}

" Clear Text: {{{1

nnoremap <silent> <Plug>EdibleClearText
      \ :<C-U>set operatorfunc=edible#clear_text<CR>g@
nnoremap <silent> <Plug>EdibleClearTextLine
      \ :<C-U>set operatorfunc=edible#clear_text
      \ <Bar> execute 'normal! '.v:count1.'g@_'<CR>
xnoremap <silent> <Plug>EdibleClearText
      \ :<C-U>call edible#clear_text(visualmode(), 1)<CR>

if !hasmapto('<Plug>EdibleClearText', 'n') && maparg('<Leader>c', 'n') ==# ''
  nmap <Leader>c <Plug>EdibleClearText
endif
if !hasmapto('<Plug>EdibleClearTextLine', 'n') &&
      \ maparg('<Leader>cc', 'n') ==# ''
  nmap <Leader>cc <Plug>EdibleClearTextLine
endif
if !hasmapto('<Plug>EdibleClearText', 'x') && maparg('<Leader>c', 'x') ==# ''
  xmap <Leader>c <Plug>EdibleClearText
endif

" }}}

" Centered Title: {{{1

command! -nargs=0 -count=1 CenteredTitle :call edible#centered_title(<count>)

nnoremap <silent> <Plug>EdibleCenterdTitle
      \ :<C-U>call edible#centered_title(v:count1)<CR>

if !hasmapto('<Plug>EdibleCenterdTitle', 'n') && maparg('gb','n') ==# ''
  nmap gb <Plug>EdibleCenterdTitle
endif

" }}}

" Fold Markers Insertion: {{{1

command! FoldMarkOpen  :call edible#fold_mark(1)
command! FoldMarkClose :call edible#fold_mark(0)

nnoremap <silent> <Plug>EdibleFoldMarkOpen  :<C-U>call edible#fold_mark(1)<CR>
nnoremap <silent> <Plug>EdibleFoldMarkClose :<C-U>call edible#fold_mark(0)<CR>

if !hasmapto('<Plug>EdibleFoldMarkOpen', 'n') && maparg('[co','n') ==# ''
  nmap [co <Plug>EdibleFoldMarkOpen
endif
if !hasmapto('<Plug>EdibleFoldMarkClose', 'n') && maparg(']co','n') ==# ''
  nmap ]co <Plug>EdibleFoldMarkClose
endif

" }}}

" Org Mode Folding {{{1

nnoremap <silent> <Tab>   :<C-u>call edible#tabbed_folding()<CR>
nnoremap <silent> <S-Tab> :<C-u>call edible#org_folding()<CR>

" }}}

" Fast Spelling Corrections: {{{1

command! FixNextSpell :call edible#lucky_fix_spell(1, <count>)
command! FixPrevSpell :call edible#lucky_fix_spell(0, <count>)

nnoremap <silent> <Plug>EdibleFixNextSpell
      \ :<C-U>call edible#lucky_fix_spell(1, v:count1)<CR>
nnoremap <silent> <Plug>EdibleFixPrevSpell
      \ :<C-U>call edible#lucky_fix_spell(0, v:count1)<CR>

if !hasmapto('<Plug>EdibleFixNextSpell', 'n') && maparg(']sf','n') ==# ''
  nmap ]sf <Plug>EdibleFixNextSpell
endif
if !hasmapto('<Plug>EdibleFixPrevSpell', 'n') && maparg('[sf','n') ==# ''
  nmap [sf <Plug>EdibleFixPrevSpell
endif

" }}}

" Grepping: {{{1

command! -nargs=* QfGrep :call edible#grep(<q-args>)

nnoremap <silent> <Plug>EdibleGrep
      \ :<C-U>set operatorfunc=edible#grep_operator<CR>g@
xnoremap <silent> <Plug>EdibleGrep
      \ :<C-U>call edible#grep_operator(visualmode())<CR>

if !hasmapto('<Plug>EdibleGrep', 'n') && maparg('<Leader>gr', 'n') ==# ''
  nmap <Leader>gr <Plug>EdibleGrep
endif
if !hasmapto('<Plug>EdibleGrep', 'x') && maparg('<Leader>gr', 'x') ==# ''
  xmap <Leader>gr <Plug>EdibleGrep
endif

" }}}

" Search With Preview: {{{1
command! SmartSearch :execute 'normal! :leftabove split<Esc>z7<CR>*'
nnoremap <silent> <Leader>* :<C-U>SmartSearch<CR>
" }}}

" Automatically Switch Relative Numbers: {{{1
augroup switch_relative_numbers
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter *
        \ if &nu && get(b:, 'relnum', 1) |
        \   setlocal relativenumber   |
        \ endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave *
        \ if &nu && get(b:, 'relnum', 1) |
        \   setlocal norelativenumber |
        \ endif
augroup END
" }}}

" Auto Closing Pairs: {{{1

" Global Variables: {{{2
if !exists('g:edible_autoclose_filetypes')
  let g:edible_autoclose_filetypes = {
    \ 'parentheses'  : [],
    \ 'squareBrace'  : [],
    \ 'curlyBrace'   : [],
    \ 'curlyNewLine' : [],
    \ 'angleBrace'   : [],
    \ 'singleQuote'  : [],
    \ 'doubleQuote'  : [],
    \ }
endif
" }}}

augroup autoclose_by_filetype " {{{2
  autocmd!
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['parentheses'],',')
        \ .' inoremap <buffer> ( ()<Esc>i'
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['squareBrace'],',')
        \ .' inoremap <buffer> [ []<Esc>i'
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['curlyBrace'],',')
        \ .' inoremap <buffer> { {}<Esc>i'
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['curlyNewLine'],',')
        \ .' inoremap <buffer> {<CR> {<CR>}<Esc>O'
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['angleBrace'],',')
        \ .' inoremap <buffer> < <><Esc>i'
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['singleQuote'],',')
        \ ." inoremap <buffer> ' ''<Esc>i"
  execute 'autocmd FileType '
        \ .join(g:edible_autoclose_filetypes['doubleQuote'],',')
        \ .' inoremap <buffer> " ""<Esc>i'
augroup END
" }}}

" }}}
