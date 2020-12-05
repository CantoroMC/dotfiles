let g:colorpicker_themes = {
      \ 'dark': {
      \   'ayu'       : 'ayu_dark',
      \   'badwolf'   : 'badwolf',
      \   'gruvbox'   : 'base16_gruvbox_dark_hard',
      \   'jellybeans': 'wombat',
      \   'molokai'   : 'base16_monokai',
      \   'PaperColor': 'base16_vim',
      \   'srcery'    : 'zenburn',
      \   },
      \ 'light': {
      \   'mayansmoke': 'xtermlight',
      \   'PaperColor': 'papercolor',
      \   },
      \ }


let g:colorpicker_light_time = [1, 2]

call colorpicker#pickIt()
