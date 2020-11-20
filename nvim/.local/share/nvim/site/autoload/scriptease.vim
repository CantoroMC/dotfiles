" Section: :Scriptnames

" Auto Loading Guards: {{{1

if exists('g:autoloaded_scriptnames')
  finish
endif
let g:autoloaded_scriptnames = 1

" }}}

function! scriptease#capture(excmd) abort
  try
    redir => out
    exe 'silent! '.a:excmd
  finally
    redir END
  endtry
  return out
endfunction

function! scriptease#scriptnames_qflist() abort
  let names = scriptease#capture('scriptnames')
  let virtual = get(g:, 'virtual_scriptnames', {})
  let list = []
  for line in split(names, "\n")
    if line =~# ':'
      let filename = expand(matchstr(line, ': \zs.*'))
      call add(list, {'text': matchstr(line, '\d\+'), 'filename': get(virtual, filename, filename)})
    endif
  endfor
  return list
endfunction
