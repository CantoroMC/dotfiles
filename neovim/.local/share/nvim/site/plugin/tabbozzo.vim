set tabline=%!YuppiDoh()

hi TabLine      ctermfg=White  ctermbg=Black     guifg=#eaeaea guibg=#151a1e
hi TabLineFill  ctermfg=White  ctermbg=Black     guifg=#151a1e guibg=#eaeaea
hi TabLineSel   ctermfg=White  ctermbg=Green     guifg=#151a1e guibg=#b8cc52

hi default link BufTabLineCurrent TabLineSel
hi default link BufTabLineActive  TabLine
hi default link BufTabLineHidden  Comment

function! YuppiDoh() abort
  let s = ''

  " Left Section: Buffer list
  let s .= ' Buffers: '

  let bufs = filter(range(1,bufnr('$')),
        \ 'buflisted(v:val) && getbufvar(v:val, "&buftype") !=? "quickfix"'
        \ )
  let curr_buf = winbufnr(0)
  for buf in bufs
    let s .= buf == curr_buf ?
          \ '%#BufTabLineCurrent#' :
          \ (bufwinnr(buf) > 0 ? '%#BufTabLineActive#' : '%#BufTabLineHidden#')
    let s .= ' %{BufLabel(' . buf.','.curr_buf. ')} '
    let s .= '%#TabLineFill# '
  endfor

  " Left Right Separator:
  let s .= '%#TabLineFill#%='

  if tabpagenr('$') > 1
    " Right Section: tab list
    " loop through each open tab page
    let s .= 'Tabs: '
    for tab in range(1,tabpagenr('$'))
      " Color Highlights
      let s .= tab == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'
      " Clickable label
      let s .= '%' .tab .'T'
      " Tab Label
      let s .= ' %{TabLabel(' . tab . ')} '
    endfor
      " Closing button only if there is more than a tab opened
      let s .= '%#TabLineFill#%999X[]'
  endif

  return s
endfunction

function! TabLabel(n) abort
  let winnr         = tabpagewinnr(a:n)
  let bufs          = tabpagebuflist(a:n)
  let nr_mod_bufs   = len(filter(copy(bufs), 'getbufvar(v:val, "&modified") isnot 0'))
  let nr_wins       = len(bufs)
  let cur_buf       = bufname(bufs[winnr - 1])

  let buf_string = (cur_buf == '') ?
        \ '[No Name]' : pathshorten(fnamemodify(cur_buf, ':p:~:.'))

  let mod_string = (nr_mod_bufs > 0) ? nr_mod_bufs.'+/' : ''
  let mod_win = '['.mod_string.nr_wins.']'

  return buf_string.' '.mod_win
endfunction

function BufLabel(b,curr) abort
    let name = bufname(a:b)
    let identifier = (name == '') ?
          \ '[No Name]' : pathshorten(fnamemodify(name, ':p:~:.'))
    let isMod = getbufvar(a:b, '&modified')

    let modifier = isMod == 1 ? ' [+]' : ''

    return ' '.identifier.modifier.' '
endfunction
