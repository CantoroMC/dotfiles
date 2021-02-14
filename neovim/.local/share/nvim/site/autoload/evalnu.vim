" autoload/easypeasy.vim

if exists('g:autoloaded_evalnu')
  finish
endif
let g:autoloaded_evalnu = 1

function! evalnu#eval_base_number(base, exp) abort
  let l:format = {
        \ 2: '0b%b',
        \ 8: '0o%o',
        \ 10: '%d',
        \ 16: '0x%x'
        \ }

  let result = eval(a:exp)
  if type(0) != type(result)
    echoerr 'The result of the given expression have to be Number'
    return ''
  endif

  return printf(l:format[a:base], result)
endfunction

function! evalnu#float_base_numbers(number_text) abort
  let l:number = str2nr(a:number_text)
  if type(0) != type(l:number)
    echoerr 'The given value '.l:number.' is NOT number.'
    return
  endif

  let l:content = [
        \ printf(' Bin: 0b%b ', l:number),
        \ printf(' Oct: 0o%o', l:number),
        \ printf(' Dec: %d', l:number),
        \ printf(' Hex: 0x%x', l:number),
        \ ]

  let l:buf = nvim_create_buf(v:false, v:true)
  call nvim_buf_set_lines(l:buf, 0, -1, v:true, l:content)
  call nvim_buf_set_keymap(l:buf, 'n', '<Esc>', '<C-W>q', {})

  let l:opts = {
        \ 'relative': 'win',
        \ 'height'  : len(l:content),
        \ 'width'   : max(map(l:content, {_, c -> len(c)})),
        \ 'row'     : 0,
        \ 'col'     : winwidth(0) - winwidth(bufwinid(l:buf)),
        \ 'style'   : 'minimal'
        \}
  let l:win = nvim_open_win(l:buf, v:true, l:opts)
  call nvim_win_set_option(l:win, 'winhl', 'Normal:Question')
endfunction

function! evalnu#get_char_encoding() abort
  redraw
  echohl MoreMsg
  echomsg 'Press any key: '
  echohl None

  let c = getchar()
  while c ==# "\<CursorHold>"
    redraw
    echohl MoreMsg
    echomsg 'Press any key: '
    echohl None
    let c = getchar()
  endwhile

  redraw
  echohl MoreMsg
  echo printf('Raw: "%s" | Char: "%s"', c, nr2char(c))
  echohl None
endfunction

function! evalnu#uuid() abort
  let r = system('uuidgen')
  let r = substitute(r, '^[\r\n\s]*\|[\r\n\s]*$', '', 'g')
  return r
endfunction
