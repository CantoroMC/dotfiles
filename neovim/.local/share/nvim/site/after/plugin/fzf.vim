" Section: Configuration

" Actions: {{{1

function! s:build_quickfix_list(lines) " {{{2
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction
" }}}

let g:fzf_action = {
      \ 'ctrl-a': function('s:build_quickfix_list'),
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-b': 'split',
      \ 'ctrl-v': 'vsplit' }
" }}}

" Layout: {{{1
let g:fzf_layout = { 'window': {
      \ 'width':      0.99,
      \ 'height':     0.50 ,
      \ 'xoffset':    0.5,
      \ 'yoffset':    1.0 ,
      \ 'border':    'top'
      \ }}
" let g:fzf_layout = { 'down': '~30%' }
" }}}

" Colors: {{{1
let g:fzf_colors = {
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Type'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'],
      \ }
" }}}

" Builtings Commands Options: {{{1
" Global
let g:fzf_command_prefix      = ''
let g:fzf_preview_window      = ['right:73%:border-left', 'CTRL-/']
" Specific
let g:fzf_buffers_jump        = 1
let g:fzf_commits_log_options =
      \ '--graph --color=always --format="%C(red)%h - %C(auto)%d %s %C(green)(%ad) %C(bold blue)<%an>"'
let g:fzf_tags_command        = 'ctags -R'
let g:fzf_commands_expect     = 'alt-enter,ctrl-x'
" }}}



" Section: Mappings
" Mapping Selecting Mappings: {{{1
nmap <Leader><Tab> <Plug>(fzf-maps-n)
imap <C-G><Tab>    <Plug>(fzf-maps-i)
xmap <Leader><Tab> <Plug>(fzf-maps-x)
omap <Leader><Tab> <Plug>(fzf-maps-o)
" }}}



" Section: User Commands

function! s:common_options(prompt, ...) abort " {{{1
  let options = [
        \ '--ansi',
        \ '--layout=reverse',
        \ '--multi',
        \ '--info=inline',
        \ '--prompt='.a:prompt.' ',
        \ '--preview-window=right:73%:border-left'
        \ ]
  if a:0 isnot 0
    let options = extend(options, a:000)
  endif
  return options
endfunction
" }}}

" Texdoc: -> run {{{1

function s:Texdoc() abort " {{{2
  let source = split(globpath(expand('$TEXMFDIST/doc/'), '**/*.pdf'))
  return map(source, "fnamemodify(v:val, ':s?'.$TEXMFDIST.'/doc/??')")
endfunction
" }}}

function! s:xdg_open_texdoc(file) abort " {{{2
  execute '!xdg-open '. $TEXMFDIST .'/doc/'.a:file. '& disown'
endfunction
" }}}

" Texdoc {{{2
command! -bang -nargs=0 Texdoc
      \ call fzf#run(fzf#wrap('texdoc', {
      \   'source': s:Texdoc(),
      \   'sink': function('<SID>xdg_open_texdoc'),
      \   'options': s:common_options(
      \     'Texdoc >',
      \     '--preview='.
      \       'pdftotext -l 10 -nopgbrk -q -- ' . $TEXMFDIST . '/doc/{} - | fmt -w $FZF_PREVIEW_COLUMNS | bat --paging=never --decorations=always --color=always --file-name ' . $TEXMFDIST . '/doc/{} --style header,grid'
      \   ),
      \   'dir': $TEXMFDIST,
      \ },
      \ <bang>0))
" }}}

" }}}

" Bins: -> run {{{1

function! s:Bins() abort " {{{2
  let l:base = expand('~/dotfiles/')
  let l:dirs = [
        \ 'shell/.local/bin/**',
        \ 'x-window/.local/bin/**',
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Bins {{{2
command! -bang -nargs=0 Bins
      \ call fzf#run(fzf#wrap({
      \   'source': s:Bins(),
      \   'sink': 'sfind',
      \   'options': s:common_options(
      \     'User Bins >',
      \     '--preview=bat --paging=never --decorations=always --color=always {}'
      \   ),
      \   'dir': '~/dotfiles/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Vim Scripts: -> run {{{1

function! s:VimData() abort " {{{2
  let l:base = expand('~/.local/share/nvim/site/')
  let l:dirs = [
        \ 'after/**',
        \ 'autoload/**',
        \ 'colors',
        \ 'compiler',
        \ 'doc',
        \ 'ftdetect',
        \ 'ftplugin/**',
        \ 'plugin/**',
        \ 'spell',
        \ 'syntax',
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*.\(vim\|txt\|\lua\)')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Vim Data {{{2
command! -bang -nargs=0 VimData
      \ call fzf#run(fzf#wrap({
      \   'source' : s:VimData(),
      \   'sink'   : 'sfind',
      \   'options': s:common_options(
      \     'Vim Scripts >',
      \     '--preview=bat --paging=never --decorations=always --color=always {}'
      \   ),
      \   'dir'    : '~/.local/share/nvim/site/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Vim Configs: -> run {{{1

function! s:VimConfigs() abort " {{{2
  let l:base = expand('~/.config/nvim/')
  let l:dirs = [
        \ '',
        \ 'lua/**',
        \ 'UltiSnips',
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*.\(vim\|\lua\|\json|\snippets)')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Vim Configs: {{{2
command! -bang -nargs=0 VimConfigs
      \ call fzf#run(fzf#wrap({
      \   'source' : s:VimConfigs(),
      \   'sink'   : 'sfind',
      \   'options': s:common_options(
      \     'Vim Configuration >',
      \     '--preview=bat --paging=never --decorations=always --color=always {}'
      \   ),
      \   'dir'    : '~/.config/nvim/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Dotfiles: -> files {{{1
command! -bang Dotfiles
      \ call fzf#vim#files(
      \ '~/dotfiles',
      \ fzf#vim#with_preview({'options': s:common_options('Dotfiles >')}),
      \ <bang>0)
" }}}

" Skeletons: -> run {{{1

function! s:read_sk_in_bf(sk) abort " {{{2
  execute '-1read ~/.config/nvim/data/skeletons/'.a:sk
endfunction
" }}}

" Fuzzy Skeletons: -> run {{{2
command! -bang -nargs=0 Skeletons
      \ call fzf#run(fzf#wrap({
      \   'source': 'ls -1 ~/.config/nvim/data/skeletons',
      \   'sink': function('<SID>read_sk_in_bf'),
      \   'options': s:common_options(
      \     'Skeletons >',
      \     '--preview=bat --paging=never --decorations=always --color=always {}'
      \   ),
      \   'dir': '~/.config/nvim/data/skeletons',
      \ },
      \ <bang>0))
" }}}

" }}}

" Git Grep: -> grep {{{1
command! -bang -nargs=* GiFGrep
      \ call fzf#vim#grep(
      \ 'git grep --line-number -- '.shellescape(<q-args>),
      \ 0,
      \ fzf#vim#with_preview({
      \   'options': s:common_options('Grep >'),
      \   'dir'    : systemlist('git rev-parse --show-toplevel')[0],
      \ }),
      \ <bang>0)
" }}}

" Bibtex: -> run {{{1

function! s:BibtexCiteSink(lines) abort " {{{2
  let r=system('bibtex-cite -mode=latex ', a:lines)
  execute ':normal! a' . r
endfunction
" }}}

" Fuzzy Bibtex Cite: {{{2
command! -bang -nargs=0 BibtexCite
      \ call fzf#run(fzf#wrap({
      \   'source' : 'bibtex-ls',
      \   'sink*'  : function('<SID>BibtexCiteSink'),
      \   'options': s:common_options('Cite > '),
      \ }
      \ ))
" }}}

function! s:BibtexMarkdownSink(lines) abort " {{{2
  let r=system('bibtex-markdown ', a:lines)
  execute ':normal! a' . r
endfunction
" }}}

" Fuzzy Markdown Cite: {{{2
command! -bang -nargs=0 MarkdownCite
      \ call fzf#run(fzf#wrap({
      \   'source' : 'bibtex-ls',
      \   'sink*'  : function('<SID>BibtexMarkdownSink'),
      \   'options': s:common_options('Cite > '),
      \ }
      \ ))
" }}}

" }}}

" vim:fdm=marker
