" autoload/colorpicker.vim

" Auto Loading Guards: {{{1

if exists('g:autoloaded_colorpicker')
  finish
endif
let g:autoloaded_colorpicker = 1

" }}}

function! colorpicker#pickIt(...) abort " {{{1

  let light_time = get(g:, 'colorpicker_light_time', [7, 14])

  " Parse Background Color: {{{2
  let bgs = ['dark', 'light']
  if a:0 >= 1 && a:0 <= 2
    let l:bg = a:1
  elseif a:0 == 0
    let l:bg = bgs[(strftime('%H') >= light_time[0] && strftime('%H') < light_time[1])
          \ ?  1 : 0]
  else
    return
  endif
  " }}}

  " Parse Color Scheme: {{{2
  let l:pairs = g:colorpicker_themes[l:bg]

  if a:0 == 2
    let l:scheme = a:2
  elseif a:0 <= 1
    let l:fpairs = filter(copy(l:pairs), 'v:val[1] > 0')
    let l:scheme = keys(l:fpairs)[s:Randn(len(keys(l:fpairs)))]
  endif
  " }}}

  " Function Execution: {{{2
  execute 'let g:airline_theme = '.string(l:pairs[l:scheme][0])
  execute 'set background='.l:bg
  execute 'colorscheme '.l:scheme
  " }}}
endfunction
" }}}

function! s:Randn(max) abort " {{{1
  return str2nr(matchstr(reltimestr(reltime()), '\v\.@<=\d+')) % a:max
endfunction
" }}}
