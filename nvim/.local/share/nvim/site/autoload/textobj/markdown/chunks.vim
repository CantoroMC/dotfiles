" textobj-markdown - Text objects for markdown {{{1
" File:          chunks.vim
" Author:        coachshea
" Maintainer:    CantoroMC
" Description:   Auto loading functionalities for textobj-markdown
" Last Modified: settembre 07, 2020
" }}}

" Fenced Code: represented with f {{{1

" Forward Selection:
function! textobj#markdown#chunks#af() abort " {{{2
  let l:tail = search('```$', 'Wc')
  let l:head = search('```\S', 'Wb')
  return !l:head || !l:tail
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}
function! textobj#markdown#chunks#if() abort " {{{2
  let l:tail = search('```$', 'Wc')
  let l:head = search('```\S', 'Wb')
  if !l:head || !l:tail
    return 0
  endif
  let l:head += 1
  let l:tail -= 1
  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}

" Backward Selection:
function! textobj#markdown#chunks#aF() abort " {{{2
  let l:head = search('```\S', 'Wbc')
  let l:tail = search('```$', 'W')
  return !l:head || !l:tail
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}
function! textobj#markdown#chunks#iF() abort " {{{2
  let l:head = search('```\S', 'Wbc')
  let l:tail = search('```$', 'W')
  if !l:head || !l:tail
    return 0
  endif
  let l:head += 1
  let l:tail -= 1
  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}

" }}}

" Text Blocks: represented with m {{{1

" Forward Selection:
function! textobj#markdown#chunks#am() abort " {{{2
  let l:tail = search('\n```\S\|\%$', 'Wc')
  if getline(l:tail) =~ '```'
    return 0
  endif

  let l:head = search('```$\n\zs\|\%^', 'Wb')
  if l:tail != line('$') && head != 1
    let l:head += 1
  endif
  if getline(l:head) =~? '```'
    return 0
  endif

  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}
function! textobj#markdown#chunks#im() abort " {{{2
  let l:tail = search('```\S', 'W')
  if !l:tail
    let l:tail = line('$')
    exe l:tail
  else
    let l:tail -= 2
  endif

  let l:head = search('```$', 'Wb')
  if !l:head
    let l:head = 1
  else
    let l:head += 2
  endif

  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}

" Backward Selection:
function! textobj#markdown#chunks#aM() abort " {{{2
  let l:head = search('```$', 'Wb')
  if !l:head
    let l:head = 1
    exe l:head
  else
    let l:head += 2
  endif

  let l:tail = search('```\S', 'W')
  if !l:tail
    let l:tail = line('$')
  else
    let l:tail -= 1
  endif

  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}
function! textobj#markdown#chunks#iM() abort " {{{2
  let l:head = search('```$', 'Wb')
  if !l:head
    let l:head = 1
    exe l:head
  else
    let l:head += 2
  endif

  let l:tail = search('```\S', 'W')
  if !l:tail
    let l:tail = line('$')
  else
    let l:tail -= 2
  endif

  return l:tail < l:head
        \ ? 0
        \ : ['V', [0, l:head, 1, 0], [0, l:tail, 1, 0]]
endfunction
" }}}

" Movements:
function! s:move(line) abort " {{{2
  if !a:line || getline(a:line) =~? '```'
    return 0
  endif
  return ['V', [0, a:line, 1, 0], [0, a:line, 1, 0]]
endfunction
" }}}

" Move forward to the begin
function! textobj#markdown#chunks#n() abort " {{{2
  return s:move(search('```$\n\zs', 'W'))
endfunction
" }}}
" Move backward to the begin
function! textobj#markdown#chunks#p() abort " {{{2
  return s:move(search('```$\n\zs\|\%^', 'Wb'))
endfunction
" }}}
" Move forward to the end
function! textobj#markdown#chunks#N() abort " {{{2
  return s:move(search('\n```\S\|\%$', 'W'))
endfunction
" }}}
" Move backward to the end
function! textobj#markdown#chunks#P() abort " {{{2
  return s:move(search('\n```\S', 'Wb'))
endfunction
" }}}

" }}}
