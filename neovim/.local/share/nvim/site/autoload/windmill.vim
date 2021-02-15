" autoload/windmill.vim

" Auto Loading Guards

if exists('g:autoloaded_windmill')
  finish
endif
let g:autoloaded_windmill = 1


" Section: Window Management

" SubSection: Golden Ratio
let s:goldenRation = 1.618034
let s:gr_auto = 0

function! windmill#golden_ratio() abort
  if !empty(getcmdwintype())
    return
  endif

  let l:mainH  = &lines / s:goldenRation
  let l:mainW  = &columns / s:goldenRation
  let l:otherH = l:mainH / s:goldenRation
  let l:otherW = l:mainW / s:goldenRation

  call s:resOtherWins(s:parallelWins(winnr()), l:otherW, l:otherH)
  call s:resMainWin(winnr(), l:mainW, l:mainH, l:otherH)
endfunction

function! windmill#toggle_golden_ratio() abort
  if s:gr_auto
    let s:gr_auto = 0
    au! GoldenRatioAug
  else
    let s:gr_auto = 1
    call s:autoCommandGoldenRatio()
    call windmill#golden_ratio()
  endif
endfunction

function! s:parallelWins(currWin) abort
  let l:windows = reverse(range(1, winnr('$')))
  return {
        \ 'width' : filter(
        \   l:windows,
        \   'winheight(v:val) == winheight(a:currWin) && v:val != a:currWin'),
        \ 'height': filter(
        \   l:windows,
        \   'winwidth(v:val) == winwidth(a:currWin) && v:val != a:currWin'),
        \ }
endfunction

function! s:resWin(Wins, otherW, otherH) abort
  if index(a:Wins.width, winnr()) >= 0
    let l:sizeW = a:otherW / len(a:Wins.width)
    execute 'vertical resize '.string(l:sizeW)
  endif
  if index(a:Wins.height, winnr()) >= 0
    let l:currH = winheight(winnr())
    if &lines - l:currH < a:otherH
      execute 'resize'
    else
      let l:sizeH = a:otherH / len(a:Wins.height)
      execute 'resize '.string(l:sizeH)
    endif
  endif
endfunction

function! s:resOtherWins(Wins, otherW, otherH) abort
  let l:currWin = winnr()
  for Win in range(1, winnr('$'))
    execute Win.'wincmd w'
    call s:resWin(a:Wins, a:otherW, a:otherH)
  endfor
  execute l:currWin.'wincmd w'
endfunction

function! s:resMainWin(win, mainW, mainH, otherH) abort
  if &lines - winheight(a:win) < a:otherH
    execute 'resize'
  else
    execute 'resize '.string(a:mainH)
  endif
  execute 'vertical resize '.string(a:mainW)
endfunction

function! s:autoCommandGoldenRatio() abort
  if s:gr_auto
    augroup GoldenRatioAug
      autocmd!
      autocmd WinEnter,BufEnter * call windmill#golden_ratio()
    augroup END
  endif
endfunction


" SubSection: More window displacement

function! windmill#toggle_window_zoom() abort
  if exists('t:zoom_winrestcmd')
    execute t:zoom_winrestcmd
    unlet t:zoom_winrestcmd
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
  endif
endfunction

function! windmill#fit_height(is_not_folded) abort
  let l:tail  = line('$')

  if a:is_not_folded
    execute 'resize '.l:tail
  else
    let l:left_col_width = &foldcolumn +
          \ (&number ? max([&numberwidth, strdisplaywidth(line('$'))]) : 0) +
          \ (&signcolumn ==# 'no' ? 0 : 2)
    let l:width = winwidth(0) - l:left_col_width

    let l:cnt   = 0
    let l:line  = 1
    while l:line <= l:tail
      let l:end = foldclosedend(l:line)
      if l:end == -1
        if &wrap
          let l:cnt += virtcol([l:line, '$']) / l:width
        endif
        let l:line += 1
      else
        let l:line = l:end + 1
      endif
      let l:cnt += 1
    endwhile
    execute 'resize '.l:cnt
  endif
endfunction

function! windmill#toggle_color_column() abort
  if !exists('b:color_column') || b:color_column == 1
    let b:cc_bg = synIDattr(synIDtrans(hlID('ColorColumn')), 'bg')
    hi ColorColumn guibg=NONE
    let b:color_column = 0
  else
    execute 'hi ColorColumn guibg='.b:cc_bg
    let b:color_column = 1
  endif
endfunction



" Section: Buffers

function! windmill#wipe_buffers() abort
  let lastbuf = bufnr('$')
  let ids = sort(filter(range(1, lastbuf), 'bufexists(v:val)'), 'n')
  execute ids[0].','.lastbuf.'bwipeout'
endfunction

function! windmill#close_buffer(stage) abort
  if(a:stage == 1)

    if(&modified)
      let answer = confirm(
            \ 'This buffer has been modified.'
            \   .'Are you sure you want to delete it?',
            \ '&Yes\n&No', 2)
      if(answer != 1)
        return
      endif
    endif

    if(!buflisted(winbufnr(0)))
      bd!
      return
    endif

    let s:buf_nr = bufnr('%')
    let s:win_nr = winnr()
    windo call windmill#close_buffer(2)
    execute s:win_nr.'wincmd w'

    let s:buflistedLeft = 0
    let s:bufFinalJump  = 0
    let l:nBufs         = bufnr('$')
    let l:i             = 1

    while(l:i <= l:nBufs)
      if(l:i != s:buf_nr)
        if(buflisted(l:i))
          let s:buflistedLeft = s:buflistedLeft + 1
        else
          if(bufexists(l:i) && !strlen(bufname(l:i)) && !s:bufFinalJump)
            let s:bufFinalJump = l:i
          endif
        endif
      endif
      let l:i = l:i + 1
    endwhile

    if(!s:buflistedLeft)
      if(s:bufFinalJump)
        windo if(buflisted(winbufnr(0))) | execute 'b! '.s:bufFinalJump | endif
      else
        enew
        let l:newBuf = bufnr('%')
        windo if(buflisted(winbufnr(0))) | execute 'b! '.l:newBuf | endif
      endif

      execute s:win_nr.'wincmd w'
    endif

    if(buflisted(s:buf_nr) || s:buf_nr == bufnr('%'))
      execute 'bd! '.s:buf_nr
    endif

    if(!s:buflistedLeft)
      set buflisted
      set bufhidden=delete
      set buftype=
      setlocal noswapfile
    endif

  else

    if(bufnr('%') == s:buf_nr)
      let prevbufvar = bufnr('#')
      if(prevbufvar > 0 && buflisted(prevbufvar) && prevbufvar != s:buf_nr)
        b #
      else
        bn
      endif
    endif
  endif
endfunction
