let s:tex_fold_envs = get(g:, 'tex_fold_envs',
      \ [ 'align',
      \   'description',
      \   'itemize',
      \   'figure',
      \   'frame',
      \   'lstlisting',
      \   'table',
      \   'tabular',
      \ ])
let s:tex_fold_sections = get(g:, 'tex_fold_sections',
      \ [ 'part',
      \   'chapter',
      \   'section',
      \   'subsection',
      \   'subsubsection',
      \   'paragraph',
      \   'subparagraph',
      \ ])
let s:sections_regex = '^\s*\\\(' . join(s:tex_fold_sections, '\|') . '\){\(.*\)}'

function! s:find_sections()
  let fold_levels = {}

  let level = 2
  for section in s:tex_fold_sections
    let i = 1
    while i <= line('$')
      if getline(i) =~# '^\s*\\' . section . '{.*'
        let fold_levels[section] = level
        let level += 1
        break
      endif
      let i += 1
    endwhile
  endfor

  return fold_levels
endfunction
let s:section_levels = s:find_sections()

function! TeXFold(lnum)
  let line = getline(a:lnum)
  if line =~# '^\s*$'
    return '-1'
  endif

  if line =~# '^\s*\\begin{document'
    return '>1'
  endif
  if line =~# '^\s*\\end{document'
    return '<1'
  endif

  for sec in keys(s:section_levels)
    if line =~# '^\s*\\'.sec
      return '>'.s:section_levels[sec]
    endif
  endfor

  let envs = '\('.join(s:tex_fold_envs, '\|').'\)'
  if line =~# '^\s*\\begin{'.envs
    return 'a1'
  endif
  if line =~# '^\s*\\end{'.envs
    return 's1'
  endif

  return '='
endfunction

function! s:tex_foldings_option() abort
  if !exists('b:tex_foldings') || b:tex_foldings == 1
    setlocal foldmethod=expr
    setlocal foldexpr=TeXFold(v:lnum)
    let b:tex_foldings = 0
  else
    setlocal foldmethod=indent
    let b:tex_foldings = 1
  endif
endfunction

nnoremap <buffer> <C-c>f :<C-U>call <SID>tex_foldings_option()<CR>
