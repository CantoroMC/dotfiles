" Section: Mappings

" Markdown View And Build : {{{1

nnoremap <buffer> <F9>   :<C-U>MarkdownPreview<CR>
nnoremap <buffer> <S-F9> :<C-U>MarkdownPreviewStop<CR>
nnoremap <buffer> <F21>  :<C-U>MarkdownPreviewStop<CR>
nnoremap <silent> <F8>   :<C-U>PandocToPdf<CR>
nnoremap <silent> <S-F8> :<C-U>RemovePdf<CR>
nnoremap <silent> <F20>  :<C-U>RemovePdf<CR>
nnoremap <silent> <F7>   :<C-U>ViewPdf<CR>

" }}}

" Markdown Headers: {{{1

function! s:MarkdownHeader(level) abort "{{{2
    if a:level == 1
        execute "normal! I# "
    elseif a:level == 2
        execute "normal! I## "
    elseif a:level == 3
        execute "normal! I### "
    elseif a:level == 4
        execute "normal! I#### "
    elseif a:level == 5
        execute "normal! I##### "
    else
        execute "normal! I###### "
    endif
endfunction
" }}}

nnoremap <buffer> <silent> <LocalLeader>h1
            \ :<C-U>call <SID>MarkdownHeader(1)<CR>
nnoremap <buffer> <silent> <LocalLeader>h2
            \ :<C-U>call <SID>MarkdownHeader(2)<CR>
nnoremap <buffer> <silent> <LocalLeader>h3
            \ :<C-U>call <SID>MarkdownHeader(3)<CR>
nnoremap <buffer> <silent> <LocalLeader>h4
            \ :<C-U>call <SID>MarkdownHeader(4)<CR>
nnoremap <buffer> <silent> <LocalLeader>h5
            \ :<C-U>call <SID>MarkdownHeader(5)<CR>
nnoremap <buffer> <silent> <LocalLeader>h6
            \ :<C-U>call <SID>MarkdownHeader(6)<CR>

" }}}

" Comments Style And Titles: {{{1

nnoremap <buffer> <LocalLeader>st a<span style="color:">** **</span><C-o>3b<C-o>l
nnoremap <buffer> <LocalLeader>tr a<a name=""></a><C-o>5h
nnoremap <buffer> <LocalLeader>co o[//]: # (Reference-Links)<CR>

" }}}

" Square Brackets Movements: {{{1

nnoremap <silent><buffer> [[ :<C-U>call search('\%(^#\{1,5\}\s\+\S\\|^\S.*\n^[=-]\+$\)', "bsW")<CR>
nnoremap <silent><buffer> ]] :<C-U>call search('\%(^#\{1,5\}\s\+\S\\|^\S.*\n^[=-]\+$\)', "sW")<CR>
xnoremap <silent><buffer> [[ :<C-U>exe "normal! gv"<Bar>call search('\%(^#\{1,5\}\s\+\S\\|^\S.*\n^[=-]\+$\)', "bsW")<CR>
xnoremap <silent><buffer> ]] :<C-U>exe "normal! gv"<Bar>call search('\%(^#\{1,5\}\s\+\S\\|^\S.*\n^[=-]\+$\)', "sW")<CR>

" }}}


" Section: Commands

command! PandocToPdf
      \ :execute '!pandoc --pdf-engine=xelatex -o '
      \ .fnameescape(expand('%:p:r')).'.pdf '
      \ .fnameescape(expand('%:p'))


" Section: Foldings

function! s:HashIndent(lnum) abort " {{{1
  let hash_header = matchstr(getline(a:lnum), '^#\{1,6}')
  if len(hash_header)
    return hash_header
  else
    let nextline = getline(a:lnum + 1)
    if nextline =~# '^=\+\s*$'
      return '#'
    elseif nextline =~# '^-\+\s*$'
      return '##'
    endif
  endif
endfunction
" }}}

function! MarkdownFoldText() abort "{{{1
  let hash_indent = s:HashIndent(v:foldstart)
  let title = substitute(getline(v:foldstart), '^#\+\s*', '', '')
  let foldsize = (v:foldend - v:foldstart + 1)
  let linecount = '['.foldsize.' lines]'
  return hash_indent.' '.title.' '.linecount
endfunction
" }}}

setlocal foldexpr=MarkdownFold()
setlocal foldmethod=expr
setlocal foldtext=MarkdownFoldText()


" Section: Options restoring

let b:undo_ftplugin = '|sil! nunmap <buffer> [[|sil! nunmap <buffer> ]]|sil! xunmap <buffer> [[|sil! xunmap <buffer> ]]'
let b:undo_ftplugin .= ' foldexpr< foldmethod< foldtext< completefunc<'
