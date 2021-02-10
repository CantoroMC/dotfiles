" Vim Filetype Plugin:
" Language:     tex,latex
" Maintainer:   Marco Cantoro
" Last Changed: Jul 26, 20

let g:tex_path = apathy#Join(map(
        \ apathy#EnvSplit($TEXMFDIST, '/usr/share/texmf-dist') +
        \ apathy#EnvSplit($TEXMFLOCAL, '/usr/share/texmf') +
        \ apathy#EnvSplit($TEXMFHOME, expand('~/.local/share/texmf')),
        \ 'v:val . "/**"'))
call apathy#Prepend('path', g:tex_path)

call apathy#Prepend('suffixesadd', '.cls,.sty,.bib,.bst')
setlocal includeexpr&
setlocal define&

call apathy#Undo()
