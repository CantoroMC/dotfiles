let g:surround_{char2nr('l')} = "\\begin{\1environment: \1}\n\t\r\n\\end{\1\r}.*\r\1}\n"
let g:surround_{char2nr('L')} = "\\\1command: \1{\r}"
let g:surround_{char2nr('_')} = "_{\r}"
let g:surround_{char2nr('^')} = "^{\r}"
let g:surround_indent = 1
