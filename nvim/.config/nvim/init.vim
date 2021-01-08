let $NVIMHOME = expand('<sfile>:p:h')

" General Settings: {{{1

" Encoding: {{{2

if has('vim_starting')
  set encoding=utf8
  set fileencodings=ucs-bom,utf-8,default,latin1
  set fileformats=unix,dos
endif
scriptencoding utf-8

" }}}

" Text: {{{2

set ambiwidth=single
set backspace=indent,eol,start
set colorcolumn+=80
set conceallevel=2
set emoji
set fillchars+=fold:-
set iskeyword+=-
set list
set listchars=tab:>-,trail:·,nbsp:+,eol:¬,precedes:←,extends:→
set matchpairs+=<:>
set matchtime=5
set nrformats=bin,hex
set scrolloff=3
set showmatch
set startofline
set synmaxcol=3000
set textwidth=80
set virtualedit=

" Wrapping: {{{3
set breakindent
set breakindentopt=min:10,shift:4,sbr
set linebreak
set sidescroll=5
set sidescrolloff=2
set showbreak=↪\
set whichwrap=b,s
set wrap
" }}}

" }}}

" Generic: {{{2

set belloff=all
set noerrorbells
set novisualbell
set clipboard+=unnamed
set confirm
set cpoptions+=$
set diffopt+=vertical,context:2,iwhite
set mouse=
set nolazyredraw
set termguicolors
set timeout
set timeoutlen=1000
set ttimeout
set ttimeoutlen=50

" }}}

" Sign Column: {{{2

set number
set numberwidth=1
set relativenumber
set signcolumn=auto

" }}}

" Command Line: {{{2

set cedit=<Home>
set cmdheight=2
if has('nvim')
  set inccommand=split
endif
set shortmess=atTcoOF
set showcmd
set wildchar=<Tab>
set wildcharm=<Tab>
set wildignore+=*.o,*.obj,*.aux,*.git,*.svn
set wildignorecase
set wildmenu
set wildmode=longest:full,full
set wildoptions=pum

" }}}

" Status And Tab Lines: {{{2

set laststatus=2
set noruler
set noshowmode
set showtabline=2

" }}}

" Windows: {{{2

set splitbelow
set splitright

" }}}

" Indentation: {{{2

set shiftround
set autoindent
set smartindent

augroup filetype_indentation " {{{3
  " TODO: add preserveindent and copyindent for tabs
  autocmd!
  " Only Spaces Four Characters:
  autocmd FileType python,matlab,help,lisp,haskell,lhaskell
        \ setl tabstop=4 softtabstop=4 expandtab shiftwidth=4
  " Only Spaces Two Characters:
  autocmd FileType vim,sh,zsh,markdown,tex,plaintex,bib,text,yaml,ruby,xml,xdefaults,tmux,cabal,nix
        \ setl tabstop=2 softtabstop=2 expandtab shiftwidth=2
  " Tabs With Two Characters:
  autocmd FileType make,json,html,css,javascript,neomuttrc,muttrc
        \ setl tabstop=2 softtabstop=2 noexpandtab shiftwidth=2
  " Tabs With Four Characters:
  autocmd FileType go,c,cpp,h
        \ setl tabstop=4 softtabstop=4 noexpandtab shiftwidth=4
  " C Indentation:
  autocmd FileType c,cpp,h
        \ setl cindent
augroup END
" }}}

" }}}

" Folding: {{{2

set foldenable
set foldcolumn=1
set foldlevelstart=0
set foldmarker={{{,}}}
set foldnestmax=10

augroup fyletype_folding_method " {{{3
  autocmd!
  " Marker
  autocmd FileType vim,tex,plaintex,text,neomuttrc,muttrc,tmux
        \ setl foldmethod=marker
  " Indentation
  autocmd FileType make,python,bib,go,json,html,css,javascript,yaml,ruby,xml,haskell,lhaskell,cabal,nix
        \ setl foldmethod=indent
  autocmd FileType lisp
        \ setl foldmethod=indent
  " Syntax
  autocmd FileType c,cpp,git,gitcommit,zsh,sh
        \ setl foldmethod=syntax
augroup END
" }}}

" }}}

" Files And Buffers: {{{2

set autoread
set autowrite
set hidden
set path+=**
set switchbuf=useopen

" }}}

" Spelling: {{{2

set nolangremap
set spelllang=en,it
set spellfile=$XDG_CONFIG_HOME/nvim/spell/dictionary.utf-8.add
set spell

" }}}

" Insert Mode Completion: {{{2

set complete+=i,kspell
set completeopt=menuone,preview,noinsert,noselect
set dictionary+="spell"
set omnifunc=syntaxcomplete#Complete
set showfulltag

" }}}

" Pop Up Menu: {{{2

if has('nvim')
  set pumblend=30
endif
set pumheight=20

" }}}

" Safety: {{{2

set nobackup
set swapfile
set updatetime=500
set writebackup

" }}}

" History: {{{2

set undodir+="$XDG_DATA_HOME/nvim/undo"
set undofile
set history=200
set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
set viewdir+="$XDG_DATA_HOME/nvim/view"
set viewoptions=cursor,curdir,folds,options

" }}}

" Searching: {{{2

set grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ %l%m
if executable('rg')
  set grepprg=rg\ -s\ --hidden\ --vimgrep
elseif executable('ag')
  set grepprg=ag\ -sR\ --hidden\ --vimgrep
elseif has('unix')
  set grepprg=grep\ -rRHnIi\ $*\ /dev/null
endif
set hlsearch
set ignorecase
set incsearch
set regexpengine=0
set smartcase
set tagcase=followscs

" }}}

" }}}

" Default Plugins And Scripts: {{{1

" Disable Unwanted Default Plugins: {{{2

let g:loaded_2html_plugin      = 1
let g:loaded_gzip              = 1
let g:loaded_tar               = 1
let g:loaded_tarPlugin         = 1
let g:loaded_zip               = 1
let g:loaded_zipPlugin         = 1
let g:loaded_tutor_mode_plugin = 1

" }}}

" Add Optional Packages: {{{2

packadd! justify
packadd! cfilter
" packadd! termdebug

" }}}

" Default Script Options: {{{2

" Plugins: {{{3

let b:match_ignorecase = 1

" }}}

" Syntax Files: {{{3

" Lisp: {{{4
let g:lisp_rainbow = 1
let g:lisp_instring = 1
let g:lispsyntax_clisp = 1
" }}}
" Markdown: {{{4
let g:markdown_fenced_languages = [
      \ 'html', 'python', 'bash=sh', 'cpp', 'c',
      \ 'matlab', 'tex', 'vim', 'help',
      \ ]
" }}  }
" Python: {{{4
let python_highlight_all = 1
" }}}
" Tex: {{{4
" Options: {{{5
" ---------------------------------
" | a |   accents/ligatures        |
" | b |   bold and italic          |
" | d |   delimiters               |
" | m |   math symbols             |
" | g |   Greek                    |
" | s |   superscripts/subscripts  |
" ---------------------------------
" }}}
let g:tex_conceal = 'admgs'
let g:tex_flavor = 'latex'
" }}}
" Shell And Zsh: {{{4
let g:sh_fold_enabled= 7
let g:zsh_fold_enable = 1
" }}}

" }}}

" }}}

" }}}

" Mappings: {{{1

" Cheat Table: {{{2

"---------------------------------------------------------------------------"
" Commands \ Modes | Normal | Insert | Command | Visual | Select | Operator |
"------------------|--------|--------|---------|--------|--------|----------|
" map  / noremap   |    @   |   -    |    -    |   @    |   @    |    @     |
" nmap / nnoremap  |    @   |   -    |    -    |   -    |   -    |    -     |
" vmap / vnoremap  |    -   |   -    |    -    |   @    |   @    |    -     |
" xmap / xnoremap  |    -   |   -    |    -    |   @    |   -    |    -     |
" smap / snoremap  |    -   |   -    |    -    |   -    |   @    |    -     |
" omap / onoremap  |    -   |   -    |    -    |   -    |   -    |    @     |
" map! / noremap!  |    -   |   @    |    @    |   -    |   -    |    -     |
" imap / inoremap  |    -   |   @    |    -    |   -    |   -    |    -     |
" cmap / cnoremap  |    -   |   -    |    @    |   -    |   -    |    -     |
"---------------------------------------------------------------------------"

" }}}

" Leader And LocalLeader: {{{2

let mapleader = ','
let maplocalleader = ' '

" }}}

" Reimplemented Keys: {{{2

" Repeat the previous macro.
nnoremap Q @@
" Disable risky :q!
nnoremap ZQ :wqall<CR>
" Closing
nnoremap <C-Q> :q<CR>
" Movement with control:
noremap <C-J> G
noremap <C-K> gg
noremap <C-H> ^
noremap <C-L> $
" Move :mode | redraw! to <C-S>
noremap <C-S> <C-L>

" Exit insert mode with ease
inoremap jk <Esc>

" Command Mode: {{{3

" Cheat Table: {{{4

"------------------------------------------------------------------------------"
" Key      | Action                                                            |
"----------|-------------------------------------------------------------------|
" c_CTRL-A | All names that match the pattern in front the cursor are inserted.|
" c_CTRL-E | Cursor to end of command-line                                     |
" c_CTRL-B | Cursor to beginning of command-line                               |
" c_CTRL-D | List names that match the pattern in front of the cursor          |
" c_CTRL-F | Default cedit key, used to open the command-line window           |
"----------|-------------------------------------------------------------------|
" c_CTRL-N | After 'wildchar' which got multiple matches, go to next match.    |
"          |   Otherwise recall more recent command-line from history.         |
" c_CTRL-P | After 'wildchar' which got multiple matches, go to previous match.|
"          |   Otherwise recall older command-line from history.               |
" c_<Up>   | Recall older command-line from history.                           |
" c_<Up>   | Recall more recent command-line from history.                     |
"------------------------------------------------------------------------------"

" }}}

" Emacs Like Movement In Command Mode:
cnoremap <C-A> <C-B>
cnoremap <C-E> <End>
cnoremap <C-D> <Del>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>
" Better Control P And N In Command Mode:
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
cnoremap <Up>   <C-P>
cnoremap <Down> <C-N>

" Send line to command line
cnoremap <C-R><C-L> <C-R>=substitute(getline('.'), '^\s*', '', '')<CR>

" }}}

" }}}

" Windows: {{{2

" Window Splitting:
nnoremap <silent> <Leader>h :<C-U>split <bar> bp<CR>
nnoremap <silent> <Leader>v :<C-U>vsplit <bar> bp<CR>
" Window Resizing
noremap <silent> <S-Left>
      \ :<C-U>execute v:count1.'wincmd <'<CR>
noremap <silent> <S-Right>
      \ :<C-U>execute v:count1.'wincmd >'<CR>
noremap <silent> <S-Up>
      \ :<C-U>execute v:count1.'wincmd +'<CR>
noremap <silent> <S-Down>
      \ :<C-U>execute v:count1.'wincmd -'<CR>
cnoremap wbd write <Bar> bdelete

" }}}

" Tabs: {{{2
nnoremap <silent> <C-W>t     :<C-u>tabnew<CR>
nnoremap <silent> <C-W><C-T> :<C-u>tabnew<CR>
nnoremap <silent> <C-W>Q     :<C-u>tabclose<CR>
nnoremap <silent> <C-N>      gt
nnoremap <silent> <C-P>      gT
" Tab version of gf
nnoremap <silent> gtf        :<C-U>execute 'tabnew'.expand('<cfile>:p')<CR>
" Tab version `<C-]>`.
nnoremap <C-]><C-T> <C-W><C-]><C-W>T
" }}}

" Some Editing Tricks: {{{2
" Substitutions with ease.
nnoremap <Leader>ra :%s///g<Left><Left><Left>
" Swap two words.
nnoremap <silent> <Leader>sw "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>
" Overwrite the current line with yanked text.
nnoremap <silent> go  pk"_dd
" Indent continuously.
vnoremap < <gv
vnoremap > >gv
" Moving lines up and down.
vnoremap K :move '<-2<CR>gv=gv
vnoremap J :move '>+1<CR>gv=gv
" Assist input normal command on visual mode.
vnoremap n :normal<Space>
" Search something in the current visual range only.
vnoremap / <Esc>/\%V
" Global substitution in visual mode.
vnoremap <C-S> "hy:%s/\V<C-R>h//g<left><left>
" }}}

" Yank And Paste: {{{2

function! s:paste_with_register(register, paste_type, paste_cmd) abort " {{{3
  let l:reg_type = getregtype(a:register)
  let l:store    = getreg(a:register)
  call setreg(a:register, l:store, a:paste_type)
  exe 'normal! "'.a:register.a:paste_cmd
  call setreg(a:register, l:store, l:reg_type)
endfunction
" }}}

function! s:yank_to_plus_operator(type, ...) abort " {{{3
  let l:sel_save = &selection
  let &selection = 'inclusive'
  let l:reg_save = @@

  if a:0
    silent exe 'normal! `<'.a:type.'`>"+y'
  elseif a:type ==# 'line'
    silent exe 'normal! `[V`]"+y'
  elseif a:type ==# 'block'
    silent exe 'normal! `[\<C-V>`]"+y'
  elseif a:type ==# 'char'
    silent exe 'normal! `[v`]"+y'
  endif

  let &selection = l:sel_save
  let @@         = l:reg_save
endfunction
" }}}

" Paste To Plus Register: {{{3
" Character Wise:
nnoremap <silent> <Leader>p
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'p')<CR>
nnoremap <silent> <Leader>P
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'P')<CR>
nnoremap <silent> <Leader>]p
      \ :<C-U>call <SID>paste_with_register('+', 'c', ']p')<CR>
nnoremap <silent> <Leader>]P
      \ :<C-U>call <SID>paste_with_register('+', 'c', ']P')<CR>
nnoremap <silent> <Leader>gp
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'gp')<CR>
nnoremap <silent> <Leader>gP
      \ :<C-U>call <SID>paste_with_register('+', 'c', 'gP')<CR>
" Line Wise:
nnoremap <silent> <Leader>lp
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'p')<CR>
nnoremap <silent> <Leader>lP
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'P')<CR>
nnoremap <silent> <Leader>l]p
      \ :<C-U>call <SID>paste_with_register('+', 'l', ']p')<CR>
nnoremap <silent> <Leader>l]P
      \ :<C-U>call <SID>paste_with_register('+', 'l', ']P')<CR>
nnoremap <silent> <Leader>lgp
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'gp')<CR>
nnoremap <silent> <Leader>lgP
      \ :<C-U>call <SID>paste_with_register('+', 'l', 'gP')<CR>
" }}}

" Copy To Plus Register: {{{3
nnoremap <silent> <Leader>y
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator<CR>g@
nnoremap <silent> <Leader>yy
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator
      \ <Bar> execute 'normal! '.v:count1.'g@_'<CR>
nnoremap <silent> <Leader>Y
      \ :<C-U>set operatorfunc=<SID>yank_to_plus_operator
      \ <Bar> execute 'normal! 0'.v:count1.'g@_'<CR>
xnoremap <silent> <Leader>y
      \ :<C-U>call <SID>yank_to_plus_operator(visualmode(), 1)<CR>
" }}}

" }}}

" }}}

" Plugins: {{{1

" Automatically Install Vim-plug If Required: {{{2
let plug_file = expand(stdpath('config').'/autoload/plug.vim')
if !filereadable(plug_file)
  execute '!curl -fLo '.plug_file.
        \ ' --create-dirs '.
        \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif
" }}}

let g:plug_window = 'vertical belowright new'
call plug#begin(stdpath('data').'/plugged')

" Utilities: {{{2

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'esamattis/slimux', {
      \ 'on' : ['SlimuxREPLConfigure', 'SlimuxShellConfigure'] }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'CantoroMC/vim-bolt'
Plug 'neomake/neomake'

" }}}

" Section: Gui {{{2

" Color schemes
Plug 'CantoroMC/ayu-vim'
Plug 'morhetz/gruvbox'
Plug 'sjl/badwolf'
Plug 'NLKNguyen/papercolor-theme'
Plug 'srcery-colors/srcery-vim'
Plug 'tomasr/molokai'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'
" Plug 'preservim/nerdtree'
Plug 'RRethy/vim-illuminate'
Plug 'Yggdroot/indentLine', { 'on': 'IndentLinesToggle' }

Plug 'mhinz/vim-startify'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }

" }}}

" Section: His Holiness {{{2

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'

" }}}

" Section: Editing {{{2

Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'CantoroMC/vim-bones'
Plug 'godlygeek/tabular'

" }}}

" Section: Filetype {{{2

" Markdown
Plug 'iamcco/markdown-preview.nvim',
      \ { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug'] }
Plug 'junegunn/vim-emoji', { 'for': ['markdown'] }

" Tex
Plug 'xuhdev/vim-latex-live-preview', { 'for': ['tex'] }

" Haskell
Plug 'neovimhaskell/haskell-vim'

" Rofi Advanced Style Information
Plug 'CantoroMC/vim-rasi'

" Nix Syntax
Plug 'LnL7/vim-nix'

" }}}

Plug 'mcchrish/nnn.vim'

" Automatically Install Missing Plugins On Startup: {{{2
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  augroup PlugEnter
    autocmd!
    autocmd VimEnter * PlugInstall
  augroup END
endif
" }}}

call plug#end()

" Source Plugins Configuration {{{2

function! s:LoadPluginPlugged(plugin_cfg_dir) abort " {{{3
  for plugin_name in keys(g:plugs)
    let plugin_cfg = fnameescape(a:plugin_cfg_dir.'/'.plugin_name.'.vim')
    if filereadable(plugin_cfg)
      execute printf('source %s', plugin_cfg)
    endif
  endfor
endfunction
" }}}

call s:LoadPluginPlugged($NVIMHOME.'/plugin.d')

" }}}

" }}}

" After Plugins: {{{1

" Format Options: {{{2
"
" Cheat Sheet: {{{3
"----------------------------------------------------------------------------"
"|  t | Auto-wrap text using textwidth                                       |
"|  c | Auto-wrap comments using textwidth, inserting the current            |
"|        comment leader automatically                                       |
"|  r | Automatically insert the current comment leader after                |
"|        hitting <Enter> in Insert mode                                     |
"|  o | Automatically insert the current comment leader after                |
"|        hitting o in Insert mode                                           |
"|  q | Allow formatting of comments with "gq".                              |
"|  w | Trailing white space indicates a paragraph continues in the next line| .
"|        A line that ends in a non-white character ends a paragraph.        |
"|  a | Automatic formatting of paragraphs.                                  |
"|  n | When formatting text, recognize numbered lists                       |
"|  2 | When formatting text, use the indent of the second line of a         |
"|        paragraph for the rest of the paragraph, instead of the indent of  |
"|        the first line.                                                    |
"|  v | Vi-compatible auto-wrapping in insert mode: Only break a line at a   |
"|        blank that you have entered during the current insert command.     |
"|  l | Long lines are not broken in insert mode                             |
"|  m | Also break at a multi-byte character above 255.                      |
"|  B | When joining lines, don't insert a space between two                 |
"|        multi-byte characters                                              |
"|  1 | Don't break a line after a one-letter word.  It's broken before it   |
"|        instead.                                                           |
"|  j | Where it make sense, remove a comment leader when joining lines      |
"----------------------------------------------------------------------------"
" }}}

augroup user_formatoptions
  autocmd!
  autocmd FileType * setl formatoptions=cqjrnBlt
augroup END

" }}}

augroup comment_strings " {{{2
  autocmd!
  autocmd FileType c,cpp,cs setlocal commentstring=//\ %s
  autocmd FileType desktop  setlocal commentstring=#\ %s
augroup END
" }}}

augroup keywordprg_filetype " {{{2
  autocmd!
  autocmd FileType ruby setl keywordprg=ri\ --format=markdown
  autocmd FileType haskell setl keywordprg=hoogle\ -q\ --info
augroup END
" }}}

" }}}

set secure
