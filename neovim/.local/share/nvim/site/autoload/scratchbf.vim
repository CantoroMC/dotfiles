let s:scratchbf_name = get(g:, 'scratchbf_name', '__Scratch__')
let s:scratchbf_pos  = get(g:, 'scratchbf_pos',  'bottom')
let s:scratchbf_size = get(g:, 'scratchbf_size', 0.25)

function! scratchbf#toggle() abort
  let l:scratch_winNr = bufwinnr(s:scratchbf_name)

  if l:scratch_winNr != -1
    call s:closeScratchWin()
  else
    call s:openScratchWin()
    call s:InitScratchWin()
  endif
endfunction

function! s:openScratchWin() abort
  let l:scratch_bufNr = bufnr(s:scratchbf_name)
  if l:scratch_bufNr != -1
    execute 'silent keepalt '.s:ScratchLook().' split '.s:scratchbf_name
  else
    execute s:ScratchLook().' new '.s:scratchbf_name
  endif
endfunction

function! s:closeScratchWin() abort
  let l:scratch_winNr = bufwinnr(s:scratchbf_name)
  if winnr() == l:scratch_winNr
    close
  else
    let l:curr_bufNr = bufnr('%')
    execute l:scratch_winNr.'wincmd w'
    close
    let l:winNr = bufwinnr(l:curr_bufNr)
    if winnr() != l:winNr
      execute l:winNr.'wincmd w'
    endif
  endif
endfunction

function! s:InitScratchWin() abort
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nospell
endfunction

function! s:ScratchLook() abort
  let l:scratch_winNr = bufwinnr(s:scratchbf_name)
  if s:scratchbf_pos ==? 'bottom'
    let l:mode = l:scratch_winNr != -1 ? '' : 'botright '
    let l:size = float2nr(floor(s:scratchbf_size * (&lines - 2)))
  elseif s:scratchbf_pos ==? 'top'
    let l:mode = l:scratch_winNr != -1 ? '' : 'topleft '
    let l:size = float2nr(floor(s:scratchbf_size * (&lines - 2)))
  elseif s:scratchbf_pos ==? 'right'
    let l:mode = l:scratch_winNr != -1 ? '' : 'botright vertical '
    let l:size = float2nr(floor(s:scratchbf_size * &columns))
  elseif s:scratchbf_pos ==? 'left'
    let l:mode = l:scratch_winNr != -1 ? '' : 'topleft vertical '
    let l:size = float2nr(floor(s:scratchbf_size * &columns))
  endif
  return l:mode.l:size
endfunction
