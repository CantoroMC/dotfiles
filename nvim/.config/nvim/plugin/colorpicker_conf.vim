let g:colorpicker_themes = {
      \ 'dark': {
      \   'ayu'       : [ 'ayu_dark', 1 ],
      \   'badwolf'   : [ 'badwolf', 1 ],
      \   'gruvbox'   : [ 'base16_gruvbox_dark_hard', 1 ],
      \   'jellybeans': [ 'wombat', 1 ],
      \   'molokai'   : [ 'base16_monokai', 0 ],
      \   'PaperColor': [ 'base16_vim', 1 ],
      \   'srcery'    : [ 'zenburn', 1 ],
      \   },
      \ 'light': {
      \   'mayansmoke': [ 'xtermlight', 1 ],
      \   'PaperColor': [ 'papercolor', 1 ],
      \   },
      \ }

let g:colorpicker_light_time = [1, 1]

call colorpicker#pickIt()
