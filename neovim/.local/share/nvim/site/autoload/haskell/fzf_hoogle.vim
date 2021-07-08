" Section: Variables
" Hoogle command
let s:hoogle_path  = get(g:, 'hoogle_path', 'hoogle')
let s:count        = get(g:, 'hoogle_count', 500)
" Fzf interface
let s:open_browser = get(g:, 'hoogle_fzf_open_browser', 'alt-f')
let s:copy_type    = get(g:, 'hoogle_fzf_copy_type',    'alt-y')
let s:copy_import  = get(g:, 'hoogle_fzf_copy_import',  'alt-i')
let s:header       = get(g:, 'hoogle_fzf_header',
      \ printf("\x1b[35m%s\x1b[m", 'enter')        . " - restart with the query\n" .
      \ printf("\x1b[35m%s\x1b[m", s:open_browser) . " - open in a browser\n" .
      \ printf("\x1b[35m%s\x1b[m", s:copy_type)    . " - copy type annotation\n" .
      \ printf("\x1b[35m%s\x1b[m", s:copy_import)  . " - copy import statement\n ")
let s:window       = get(g:, 'hoogle_fzf_window', {'window': 'call haskell#fzf_hoogle#floatwindow(50, 132)'})
let s:fzf_preview  = get(g:, 'hoogle_fzf_preview', 'right:60%:border-left:wrap')
" Miscellaneous
let s:open_tool       = get(g:, 'hoogle_open_link', executable('xdg-open') ? 'xdg-open' : '')
let s:enable_messages = get(g:, 'hoogle_enable_messages', v:true)
let s:preview_handler = expand('<sfile>:h') .. '/preview'
let s:cache_file      = get(g:, 'hoogle_fzf_cache_file', stdpath('cache') .. '/fzf-hoogle_cache.json')



" Section: Script Functions
function! s:Source(query) abort
  let hoogle = printf('%s --json --count=%s %s 2> /dev/null | ', s:hoogle_path, s:count, shellescape(a:query))
  let jq_stream = "jq -cn --stream 'fromstream(1|truncate_stream(inputs))' 2> /dev/null | "
  let add_path = "jq -c '. | setpath([\"fzfhquery\"]; if .module.name == null then .item else .module.name + \" \" + .item end)' | "
  let remove_duplicates = "awk -F 'fzfhquery' '!seen[$NF]++' | "
  let save_file = 'tee ' .. s:cache_file .. ' | '
  let fzf_lines = "jq -r '.fzfhquery' | "
  let awk_orange = '{ printf "\033[33m"$1"\033[0m"; $1=""; print $0 }'
  let awk_green = '{ printf "\033[32m"$1"\033[0m"; $1=""; print $0 }'
  let colorize = printf("awk '{ if ($1 == \"package\" || $1 == \"module\") %s else %s }'", awk_orange, awk_green)
  return hoogle .. jq_stream .. add_path .. remove_duplicates .. save_file .. fzf_lines .. colorize
endfunction

function! s:EchoMsg(text) abort
  redraw!
  if s:enable_messages
    echohl WarningMsg
    echomsg 'fzf-hoogle: ' . a:text
    echohl None
  endif
endfunction

function! s:Handler(bang, lines) abort
  " exit if empty for <Esc> hit
  if a:lines == [] || a:lines == ['','','']
    return
  endif

  let keypress = a:lines[1]
  if keypress ==? 'enter'
    let query = a:lines[0]
    call haskell#fzf_hoogle#run(query, a:bang)
    call feedkeys('i', 'n')
    return
  elseif len(a:lines) > 2
    if keypress ==? s:open_browser
      let item = a:lines[2]
      let link = trim(system(
            \   printf("jq -r --arg a \"%s\" '. | select(.fzfhquery == \$a) | .url' %s",
            \     item, s:cache_file)
            \ ))
      call s:EchoMsg('The link was sent to default browser')
      silent call system(s:open_tool .. " " .. shellescape(link, 1))
    elseif keypress ==? s:copy_type
      let type = substitute(a:lines[2], '.\{-}\s\ze.*', '', '')
      call s:EchoMsg('The type annotation was copied')
      call setreg('*', type, 'l')
    elseif keypress ==? s:copy_import
      let alist = split(a:lines[2])
      let import = 'import ' .. alist[0] .. ' (' .. alist[1] .. ')'
      call s:EchoMsg('The import statement was copied')
      call setreg('*', import, 'l')
    endif
  endif
endfunction



" Section: Hoogle-fzf Interface
function! haskell#fzf_hoogle#floatwindow(lines, columns) abort
  let v_pos = float2nr((&lines - a:lines) / 2)
  let h_pos = float2nr((&columns - a:columns) / 2)
  let opts = {
      \ 'relative': 'editor',
      \ 'row':       v_pos,
      \ 'col':       h_pos,
      \ 'height':    a:lines,
      \ 'width':     a:columns,
      \ 'border': 'double',
      \ 'style': 'minimal'
      \ }
  let buf = nvim_create_buf(v:false, v:true)
  call nvim_open_win(buf, v:true, opts)
endfunction

function! haskell#fzf_hoogle#run(query, fullscreen) abort
  let prompt = strdisplaywidth(a:query) > 30 ? a:query[:27] .. '.. > ' : a:query .. ' > '
  let options = {
      \ 'sink*': function('s:Handler', [a:fullscreen]),
      \ 'source': s:Source(a:query),
      \ 'options': [
            \ '--no-multi',
            \ '--print-query',
            \ printf('--expect=enter,%s,%s,%s', s:open_browser, s:copy_type, s:copy_import),
            \ '--tiebreak=begin',
            \ '--ansi',
            \ '--exact',
            \ '--inline-info',
            \ '--prompt', prompt,
            \ '--header', s:header,
            \ '--preview', printf('%s %s {} {n}', s:preview_handler, s:cache_file),
            \ '--preview-window', s:fzf_preview,
            \ ]
      \ }
  call extend(options, s:window)

  call fzf#run(fzf#wrap('hoogle', options, a:fullscreen))
endfunction
