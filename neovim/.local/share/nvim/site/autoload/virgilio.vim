" Section: Auto Loading Guards

if exists('g:autoloaded_virgilio')
  finish
endif
let g:autoloaded_virgilio = 1

" Section: Key

function! virgilio#lookup(...) abort " {{{1
  let result = virgilio#key_runner()
  let bufname = printf('virgilio: $%s', toupper(result.args[0]))
  if a:0 == 0
    let channel = 'buffer'
  else
    let channel = a:000[0]
  endif
  execute 'call '.printf('virgilio#channels#%s#create(bufname, result.output)', channel)
endfunction

function! virgilio#key_runner() abort " {{{1
  let lookfor = extend(
        \ split(&keywordprg, '\s\+'),
        \ [expand('<cword>')]
        \ )
  let output = virgilio#process(lookfor)
  return {
        \ 'args':   lookfor,
        \ 'output': output
        \ }
endfunction

" Section: Process

function! virgilio#process(args) abort
  let job_options = {
        \ 'stdout': [''],
        \ 'on_stdout': funcref('s:job_cb'),
        \ 'on_exit': funcref('s:job_cb'),
        \ }

  let job = jobstart(a:args, job_options)
  call jobwait([job])
  let output = join(job_options.stdout, "\n")

  return split(output, '\r\?\n')
endfunction

function! s:job_cb(job_id, data, event) abort dict
  if a:event ==# 'stdout'
    let self.stdout[-1] .= a:data[0]
    call extend(self.stdout, a:data[1:])
  else
    let self.exitval = a:data
  endif
endfunction
