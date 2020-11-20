let NERDTreeNaturalSort     = 1
let NERDTreeSortOrder       = ['\/$', '*', '[[extension]]']
let NERDTreeChDirMode       = 2
let NERDTreeShowHidden      = 1
let NERDTreeShowLineNumbers = 1
let NERDTreeQuitOnOpen      = 1
let NERDTreeWinSize         = 28
let NERDTreeStatusLine      = 'airline-nerdtree'
let NERDTreeIgnore          = [
      \ '\.o$', '__pycache__','.aux$', '.hi'
      \ ]

nnoremap <silent> <F2> :NERDTreeToggle<CR>
