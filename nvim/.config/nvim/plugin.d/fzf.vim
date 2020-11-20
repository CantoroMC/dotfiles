" Configuration: {{{1

function! s:build_quickfix_list(lines) " {{{2
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction
" }}}

" Fuzzy Finder Actions: {{{2
let g:fzf_action = {
      \ 'ctrl-q': function('s:build_quickfix_list'),
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit' }
" }}}

" Layout: {{{2
let g:fzf_layout = { 'down': '~30%' }

" For Floating Windows: {{{3
" let g:fzf_layout = { 'window': {
"       \ 'width':      0.7,
"       \ 'height':     0.7 ,
"       \ 'xoffset':    0.5,
"       \ 'yoffset':    0.5 ,
"       \ 'highlight': 'Comment' ,
"       \ 'border':    'sharp'
"       \ }}
" }}}
" }}}

" Fuzzy Finder Colors: {{{
let g:fzf_colors = {
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'],
      \ }
" }}}

" Fuzzy Finder Scripts: {{{2
let g:fzf_command_prefix = 'Fuzzy'
let g:fzf_preview_window = ''
let g:fzf_buffers_jump   = 1
let g:fzf_tags_command   = 'ctags -R'
" }}}

" }}}

" Mappings: {{{1

execute 'nnoremap <silent> <M-f>b :<C-u>'.g:fzf_command_prefix.'Buffers<CR>'
execute 'nnoremap <silent> <M-f>f :<C-u>'.g:fzf_command_prefix.'Files<CR>'
execute 'nnoremap <silent> <M-f>g :<C-u>'.g:fzf_command_prefix.'GFiles<CR>'
execute 'nnoremap <silent> <M-f>h :<C-u>'.g:fzf_command_prefix.'History<CR>'
execute 'nnoremap <silent> <M-f>l :<C-u>'.g:fzf_command_prefix.'BLines<CR>'
execute 'nnoremap <silent> <M-f>L :<C-u>'.g:fzf_command_prefix.'Lines<CR>'
execute 'nnoremap <silent> <M-f>r :<C-u>'.g:fzf_command_prefix.'Rg<CR>'
execute 'nnoremap <silent> <M-f>t :<C-u>'.g:fzf_command_prefix.'Tags<CR>'

" Selecting Mappings:
nmap <Leader><Tab> <Plug>(fzf-maps-n)
imap <C-G><Tab>    <Plug>(fzf-maps-i)
xmap <Leader><Tab> <Plug>(fzf-maps-x)
omap <Leader><Tab> <Plug>(fzf-maps-o)

nnoremap <silent> <M-f>B :<C-u>FuzzyBins<CR>
nnoremap <silent> <M-f>c :<C-u>FuzzyBibtexCite<CR>
nnoremap <silent> <M-f>d :<C-u>FuzzyDotfiles<CR>
nnoremap <silent> <M-f>p :<C-u>FuzzyVimPlugins<CR>
nnoremap <silent> <M-f>s :<C-u>FuzzySkeletons<CR>
nnoremap <silent> <M-f>v :<C-u>FuzzyVimConfig<CR>

nnoremap <silent> <M-f>  :<C-u>FuzzyHelper<CR>

" }}}

" User Commands: {{{1

function! s:parse_options(prompt, ...) abort " {{{2
  let options = [
        \ '--ansi',
        \ '--layout=reverse',
        \ '--multi',
        \ '--info=inline',
        \ '--prompt='.a:prompt.' ',
        \ ]
  if a:0 isnot 0
    let options = extend(options, a:000)
  endif
  return options
endfunction
" }}}

" Dotfiles: {{{2

function! s:Bins() abort " {{{3
  let l:base = expand('~/dotfiles/')
  let l:dirs = [
        \ 'shell/.local/bin/**',
        \ "x-window/.local/bin/**",
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Bins {{{3
command! -bang -nargs=0 FuzzyBins
      \ call fzf#run(fzf#wrap({
      \   'source' : s:Bins(),
      \   'sink'   : 'sfind',
      \   'options': s:parse_options('User Bins >'),
      \   'down'   : '30%',
      \   'dir'    : '~/dotfiles/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Vim Scripts: {{{2

function! s:VimConfig() abort " {{{3
  let l:base = expand('~/dotfiles/nvim/.local/share/nvim/site/')
  let l:dirs = [
        \ 'after/**',
        \ 'autoload/**',
        \ 'colors',
        \ 'compiler',
        \ 'doc',
        \ 'ftdetect',
        \ 'ftplugin/**',
        \ 'plugin/**',
        \ 'syntax',
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*.\(vim\|txt\)')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Vim Config {{{3
command! -bang -nargs=0 FuzzyVimConfig
      \ call fzf#run(fzf#wrap({
      \   'source' : s:VimConfig(),
      \   'sink'   : 'sfind',
      \   'options': s:parse_options('Vim Scripts >'),
      \   'down'   : '30%',
      \   'dir'    : '~/dotfiles/nvim/.local/share/nvim/site/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Vim Plugins: {{{2

function! s:VimPlugins() abort " {{{3
  let l:base = expand('~/dotfiles/nvim/.config/nvim/')
  let l:dirs = [
        \ 'plugin.d',
        \ 'plugin',
        \ 'UltiSnips',
        \ ]
  let l:dirs_joined = join(map(l:dirs, 'l:base.v:val'),',')
  return map(
        \ split(globpath(l:dirs_joined, '*.\(vim\|snippets\)')),
        \ 'fnamemodify(v:val, ":s?". l:base ."??")'
        \ )
endfunction
" }}}

" Fuzzy Vim Plugins: {{{3
command! -bang -nargs=0 FuzzyVimPlugins
      \ call fzf#run(fzf#wrap({
      \   'source' : s:VimPlugins(),
      \   'sink'   : 'sfind',
      \   'options': s:parse_options('Plugin Configuration >'),
      \   'down'   : '30%',
      \   'dir'    : '~/dotfiles/nvim/.config/nvim/',
      \ },
      \ <bang>0))
" }}}

" }}}

" Dotfiles: {{{2
command! -bang FuzzyDotfiles
      \ call fzf#vim#files('~/dotfiles',
      \ {
      \   'options': s:parse_options('Dotfiles >'),
      \   'down'   : '30%',
      \ },
      \ <bang>0)
" }}}

" Git Grep: {{{2
command! -bang -nargs=* FuzzyGitGrep
      \ call fzf#vim#grep(
      \ 'git grep --line-number -- '.shellescape(<q-args>),
      \ 0,
      \ {
      \   'dir'    : systemlist('git rev-parse --show-toplevel')[0],
      \   'options': s:parse_options('Grep >', '--preview-window=right:10%'),
      \   'down'   : '30%',
      \ },
      \ <bang>0)
" }}}

" Bibtex: {{{2

function! s:BibtexCiteSink(lines) abort " {{{3
  let r=system('bibtex-cite -mode=latex ', a:lines)
  execute ':normal! a' . r
endfunction
" }}}

" Fuzzy Bibtex Cite: {{{3
command! -bang -nargs=0 FuzzyBibtexCite
      \ call fzf#run({
      \   'source' : 'bibtex-ls',
      \   'sink*'  : function('<SID>BibtexCiteSink'),
      \   'options': s:parse_options('Cite > ',
      \       '--color=bg+:#F3F3F3,bg:#FAFAFA,spinner:#FF7733,hl:#ABB0B6,'
      \       .'fg:#5C6773,header:#ABB0B6,info:#E6B673,pointer:#FF7733,'
      \       .'marker:#FF7733,fg+:#5C6773,prompt:#FF7733,hl+:#FF7733'),
      \   'down'   : '30%',
      \ })
" }}}

function! s:BibtexMarkdownSink(lines) abort " {{{3
  let r=system('bibtex-markdown ', a:lines)
  execute ':normal! a' . r
endfunction
" }}}

" Fuzzy Markdown Cite: {{{3
command! -bang -nargs=0 FuzzyMarkdownCite
      \ call fzf#run({
      \   'source' : 'bibtex-ls',
      \   'sink*'  : function('<SID>BibtexMarkdownSink'),
      \   'options': s:parse_options('Cite > ',
      \       '--color=bg+:#F3F3F3,bg:#FAFAFA,spinner:#FF7733,hl:#ABB0B6,'
      \       .'fg:#5C6773,header:#ABB0B6,info:#E6B673,pointer:#FF7733,'
      \       .'marker:#FF7733,fg+:#5C6773,prompt:#FF7733,hl+:#FF7733'),
      \   'down'   : '30%',
      \ })
" }}}

" }}}

" Templates: {{{2

function! s:read_template_into_buffer(template) abort " {{{3
  execute '-1read ~/dotfiles/nvim/.local/share/nvim/templates/'.a:template
endfunction
" }}}

" Fuzzy Skeletons: {{{3
command! -bang -nargs=0 FuzzySkeletons
      \ call fzf#run(fzf#wrap({
      \   'source': 'ls -1 ~/dotfiles/nvim/.local/share/nvim/templates',
      \   'sink': function('<SID>read_template_into_buffer'),
      \   'options': s:parse_options('Skeletons > '),
      \   'down': '30%',
      \   'dir': '~/dotfiles/nvim/.local/share/nvim/templates',
      \ },
      \ <bang>0))
" }}}

" }}}

" Mapped Fuzzy Commands: {{{2

" Parse Mapped Commands: {{{3
let s:fzf_commands = [
      \ '(b)  '.g:fzf_command_prefix.'Buffers',
      \ '(f)  '.g:fzf_command_prefix.'Files'  ,
      \ '(g)  '.g:fzf_command_prefix.'GFiles' ,
      \ '(h)  '.g:fzf_command_prefix.'History',
      \ '(l)  '.g:fzf_command_prefix.'BLines' ,
      \ '(L)  '.g:fzf_command_prefix.'Lines'  ,
      \ '(r)  '.g:fzf_command_prefix.'Rg'     ,
      \ '(t)  '.g:fzf_command_prefix.'Tags'   ,
      \ ]

let s:fzf_commands = sort(extend(s:fzf_commands, [
      \ '(B)  FuzzyBins',
      \ '(c)  FuzzyBibtexCite',
      \ '(d)  FuzzyDotfiles',
      \ '(p)  FuzzyVimPlugins',
      \ '(s)  FuzzySkeletons',
      \ '(v)  FuzzyVimConfig',
      \ ]), 'i')
" }}}

function! s:FuzzyHelper(item) abort " {{{3
  let command = substitute(a:item, '(\w*)\s*', '', 'e')
   execute ':'.command
endfunction
" }}}

" Fuzzy Helper: {{{3
command! -bang -nargs=0 FuzzyHelper
      \ call fzf#run(fzf#wrap({
      \   'source' : s:fzf_commands,
      \   'sink'   : function('<SID>FuzzyHelper'),
      \   'options': s:parse_options('Mapping Helper >'),
      \   'down'   : '30%',
      \ },
      \ <bang>0))
" }}}

" }}}

" }}}
