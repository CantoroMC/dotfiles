" Section: General Settings

" Encoding: {{{1

if has('vim_starting')
  set encoding=utf-8
  set fileencodings=ucs-bom,utf-8,default,latin1
  set fileformats=unix,dos
endif
scriptencoding utf-8

" }}}

" Text: {{{1

set ambiwidth=single
set backspace=indent,eol,start
set colorcolumn+=80
set conceallevel=2
set emoji
set fillchars+=fold:•,eob:~
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

" Wrapping: {{{2
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

" Generic: {{{1

set belloff=all
set noerrorbells
set novisualbell
set clipboard+=unnamed
set confirm
set cpoptions+=$
set diffopt+=vertical,context:2,iwhite
set modelines=2
set mouse=a
set nolazyredraw
set termguicolors
set timeout
set timeoutlen=1000
set ttimeout
set ttimeoutlen=50

" }}}

" Sign Column: {{{1

set number
set numberwidth=1
set relativenumber
set signcolumn=auto

" }}}

" Command Line: {{{1

set cedit=<Home>
set cmdheight=1
set inccommand=split
set shortmess=atTcoOF
set showcmd
set wildchar=<Tab>
set wildcharm=<Tab>
set wildignore+=*~,*.o,*.obj,*.aux,*.git,*.svn,*.pyc,__pycache__
set wildignorecase
set wildmenu
set wildmode=longest:full,full
set wildoptions=pum

" }}}

" Status And Tab Lines: {{{1

set laststatus=2
set ruler
set noshowmode
set showtabline=1

" }}}

" Windows: {{{1

set equalalways
set splitbelow
set splitright

" }}}

" Indentation: {{{1

set shiftround
set autoindent
set smartindent

augroup filetype_indentation " {{{2
  " TODO: add preserveindent and copyindent for tabs
  autocmd!
  " Only Spaces Four Characters:
  autocmd FileType
        \ python,matlab,help,lisp,haskell,lhaskell,config,conf
        \ setl tabstop=4 softtabstop=4 expandtab shiftwidth=4
  " Only Spaces Two Characters:
  autocmd FileType
        \ vim,sh,zsh,markdown,tex,plaintex,bib,text,yaml,ruby,xml,xdefaults,tmux,cabal,nix,lua
        \ setl tabstop=2 softtabstop=2 expandtab shiftwidth=2
  " Tabs With Two Characters:
  autocmd FileType
        \ make,json,html,css,javascript,neomuttrc,muttrc
        \ setl tabstop=2 softtabstop=2 noexpandtab shiftwidth=2
  " Tabs With Four Characters:
  autocmd FileType
        \ go,c,cpp,h
        \ setl tabstop=4 softtabstop=4 noexpandtab shiftwidth=4
  " C Indentation:
  autocmd FileType
        \ c,cpp,h
        \ setl cindent
augroup END
" }}}

" }}}

" Folding: {{{1

set foldenable
set foldcolumn=1
set foldlevelstart=0
set foldmarker={{{,}}}
set foldnestmax=10
set foldtext=fooldme#foldtext()

augroup filetype_folding_method " {{{2
  autocmd!
  " Marker
  autocmd FileType
        \ text,neomuttrc,muttrc,tmux
        \ setl foldmethod=marker
  " Indentation
  autocmd FileType
        \ make,python,bib,go,json,html,css,javascript,yaml,ruby,xml,haskell,lhaskell,cabal,nix,lua,lisp,zsh,vim,config,conf,tex,plaintex
        \ setl foldmethod=indent
  " Syntax
  autocmd FileType
        \ c,cpp,git,gitcommit,sh
        \ setl foldmethod=syntax
augroup END
" }}}

" }}}

" Files And Buffers: {{{1

set autoread
set autowrite
set hidden
set path+=**
set switchbuf=useopen,usetab

" }}}

" Spelling: {{{1

set nolangremap
set spelllang=en,it
set spellfile=$XDG_CONFIG_HOME/nvim/spell/dictionary.utf-8.add
set spelloptions=camel
set nospell


" Enable Spelling
augroup spell_for_textual
  autocmd!
  autocmd FileType 
        \ tex,plaintex,context,bibtex,text,gitcommit
        \ setl spell
augroup END
" }}}

" Insert Mode Completion: {{{1

set complete+=i,kspell
set completeopt=menuone,preview,noinsert,noselect
set dictionary+="spell"
set omnifunc=syntaxcomplete#Complete
set showfulltag

" }}}

" Pop Up Menu: {{{1

set pumblend=30
set pumheight=20

" }}}

" Safety: {{{1

set nobackup
set swapfile
set updatetime=300
set writebackup

" }}}

" History: {{{1

set undodir+="$XDG_DATA_HOME/nvim/undo"
set undofile
set history=200
set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
set viewdir+="$XDG_DATA_HOME/nvim/view"
set viewoptions=cursor,curdir,folds,options

" }}}

" Searching: {{{1

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

filetype plugin indent on
syntax enable


" Section: ftplugin settings and more autocommands

" Format Options: {{{1
"
" Cheat Sheet: {{{2
" jcroql is the default.
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
  autocmd FileType * setl formatoptions-=o
  autocmd FileType * setl formatoptions-=t
  autocmd FileType * setl formatoptions+=nB
  " t: is useful sometimes and annoying in others
augroup END
" }}}

augroup comment_strings " {{{1
  autocmd!
  autocmd FileType c,cpp,cs setlocal commentstring=//\ %s
  autocmd FileType desktop  setlocal commentstring=#\ %s
augroup END
" }}}

augroup keywordprg_filetype " {{{1
  autocmd!
  autocmd FileType ruby setl keywordprg=ri\ --format=markdown
  autocmd FileType haskell setl keywordprg=hoogle\ -q\ --info
augroup END
" }}}

augroup highlight_on_yank " {{{1
  autocmd!
  autocmd TextYankPost *
        \ lua vim.highlight.on_yank({higroup="IncSearch", on_visual = false})
augroup END
" }}}


" Section: Distribution Plugins And Scripts Options

" Disable Unwanted Plugins: {{{1

let g:loaded_2html_plugin      = 1
let g:loaded_gzip              = 1
let g:loaded_tar               = 1
let g:loaded_tarPlugin         = 1
let g:loaded_zip               = 1
let g:loaded_zipPlugin         = 1
let g:loaded_tutor_mode_plugin = 1

" }}}

" Add Optional Packages: {{{1

" Text justification
packadd! justify
" Quickfix list filtering
packadd! cfilter
" Terminal debugger
" packadd! termdebug
" Make life easier with plugins
" packadd! vimball

" }}}

" Script Variables: {{{1

" Plugins: {{{2

" Matchit
let b:match_ignorecase = 1

" }}}

" Syntax Files: {{{2

" Lisp: {{{3
let g:lisp_rainbow = 1
let g:lisp_instring = 1
let g:lispsyntax_clisp = 1
" }}}

" Markdown: {{{3
let g:markdown_fenced_languages = [
      \ 'html', 'python', 'bash=sh', 'cpp', 'c',
      \ 'matlab', 'tex', 'vim', 'help',
      \ ]
" }}}

" Python: {{{3
let python_highlight_all = 1
" }}}

" Tex: {{{3
" Conceal Options: {{{4
" ---------------------------------
" | a |   accents/ligatures        |
" | b |   bold and italic          |
" | d |   delimiters               |
" | m |   math symbols             |
" | g |   Greek                    |
" | s |   superscripts/subscripts  |
" ---------------------------------
" }}}
let g:tex_conceal = 'abdmgs'
let g:tex_flavor = 'latex'
" }}}

" Vim: {{{3
" Embedded script highlighting supported ((Lua,Py,Ruby) or 0 for disabling)
let g:vimsyn_embed = 'lPr'
" folding for syntax (augroup, fold fun, Python)
let g:vimsyn_folding = 'afP'
" }}}

" Shell And Zsh: {{{3
let g:sh_fold_enabled= 7
" let g:zsh_fold_enable = 1
" }}}

" }}}

" }}}

" vim:fdm=marker
