let NERDTreeNaturalSort     = 1
let NERDTreeSortOrder       = ['\/$', '*', '[[extension]]']
let NERDTreeChDirMode       = 2
let NERDTreeShowHidden      = 1
let NERDTreeShowLineNumbers = 1
let NERDTreeQuitOnOpen      = 1
let NERDTreeWinSize         = 28
let NERDTreeStatusLine      = 'airline-nerdtree'
let NERDTreeIgnore          = [
      \ '\.hi$', '\.o$', '__pycache__','\.aux$'
      \ ]

nnoremap <silent> <F2> :<C-U>NERDTreeToggle<CR>
