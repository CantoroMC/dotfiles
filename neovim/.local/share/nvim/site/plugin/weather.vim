" plugin/picasso.vim
if exists('g:loaded_weather')
  finish
endif
let g:loaded_weather = 1

let g:loaded_picasso = 1
command! -bang -nargs=? Weather
      \ :lua require'mc.plugin.weather'.show_weather(<f-args>)
