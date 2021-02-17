" plugin/edible.vim
" File: edible
" Author: Marco Cantoro
" Description: text editing utilities
" Last Modified: agosto 28, 2020

if exists('g_loaded_edible')
  finish
endif
let g:loaded_edible = 1


" Section: Text Editing Tricks

" Reimplemented i_CTRL-Y and i_CTRL-E
inoremap <silent> <C-Y> <C-R><C-R>=edible#look_up_or_down(0)<CR>
inoremap <silent> <C-E> <C-R><C-R>=edible#look_up_or_down(1)<CR>

" Remove trailing whitespace around the current line or in the whole buffer
command! -bang FuckWhite :call edible#strip_trailing_spaces(<q-bang>)
command! -bang FW FuckWhite<bang>
nnoremap <silent> <Plug>EdibleFuckWhite :<C-U>FuckWhite<CR>
if !hasmapto('<Plug>EdibleFuckWhite', 'n') && maparg('<LocalLeader>fw', 'n') ==# ''
  nmap <Leader>fw <Plug>EdibleFuckWhite
endif

augroup edible_strip_trailing_spaces
  autocmd!
  autocmd BufWritePre * FuckWhite
augroup END

" Strip Blank Lines
command! StripBlankLines :call edible#strip_blank_lines()

" Re Indent Whole Buffer
command! ReindentWhole :execute 'normal! mzgg=G`z'
nnoremap <silent> <Plug>EdibleReindentWhole :<C-U>ReindentWhole<CR>
if !hasmapto('<Plug>EdibleReindentWhole', 'n') && maparg('<Leader>=', 'n') ==# ''
  nmap <Leader>= <Plug>EdibleReindentWhole
endif

" Title Case
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

" Clear Text
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
if !hasmapto('<Plug>EdibleClearTextLine', 'n') && maparg('<Leader>cc', 'n') ==# ''
  nmap <Leader>cc <Plug>EdibleClearTextLine
endif
if !hasmapto('<Plug>EdibleClearText', 'x') && maparg('<Leader>c', 'x') ==# ''
  xmap <Leader>c <Plug>EdibleClearText
endif

" Centered Title
command! -nargs=0 -count=1 CenteredTitle :call edible#centered_title(<count>)
nnoremap <silent> <Plug>EdibleCenterdTitle
      \ :<C-U>call edible#centered_title(v:count1)<CR>
if !hasmapto('<Plug>EdibleCenterdTitle', 'n') && maparg('gb','n') ==# ''
  nmap gb <Plug>EdibleCenterdTitle
endif

" Fast Spelling Corrections
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

" Search With Preview
command! SmartSearch :execute 'normal! :leftabove split<Esc>z7<CR>*'
nnoremap <silent> <Leader>* :<C-U>SmartSearch<CR>

" Automatically Switch Relative Numbers
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

" Auto Closing Pairs
let s:autopair_ft = get(g:, 'edible_autopair_fts',
      \ {
      \   'parentheses' : ['vim', 'c', 'cpp', 'tex', 'sh', 'ruby', 'zsh', 'haskell', 'lua'],
      \   'squareBrace' : ['vim', 'c', 'cpp', 'tex', 'sh', 'ruby', 'zsh', 'haskell', 'lua'],
      \   'curlyBrace'  : ['vim', 'ruby', 'zsh', 'ruby', 'sh', 'bash', 'haskell', 'tex', 'lua'],
      \   'curlyNewLine': ['vim', 'sh', 'c', 'cpp', 'tex', 'zsh', 'ruby', 'lua'],
      \   'angleBrace'  : [],
      \   'singleQuote' : ['vim', 'c', 'cpp', 'sh','ruby', 'zsh', 'lua'],
      \   'doubleQuote' : ['c', 'cpp','ruby', 'tex', 'zsh', 'sh', 'bash', 'haskell', 'lua'],
      \ })

augroup autoclose_by_filetype
  autocmd!
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['parentheses'],',')
        \ .' inoremap <buffer> ( ()<Esc>i'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['squareBrace'],',')
        \ .' inoremap <buffer> [ []<Esc>i'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['curlyBrace'],',')
        \ .' inoremap <buffer> { {}<Esc>i'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['curlyNewLine'],',')
        \ .' inoremap <buffer> {<CR> {<CR>}<Esc>O'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['angleBrace'],',')
        \ .' inoremap <buffer> < <><Esc>i'
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['singleQuote'],',')
        \ ." inoremap <buffer> ' ''<Esc>i"
  execute 'autocmd FileType '
        \ .join(s:autopair_ft['doubleQuote'],',')
        \ .' inoremap <buffer> " ""<Esc>i'
augroup END


" Section: Grep with a qflist

command! -nargs=* Grep :call edible#grep(<q-args>)

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
