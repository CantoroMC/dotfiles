let g:amivgo_freq_cfgs = {
      \ 'git'         : 'dotfiles/git/.config/git/config',
      \ 'neomutt'     : 'dotfiles/mail/.config/neomutt/neomuttrc',
      \ 'nvim'        : 'dotfiles/nvim/.config/nvim/init.vim',
      \ 'rofi'        : 'dotfiles/x-window/.config/rofi/config.rasi',
      \ 'tmux'        : 'dotfiles/shell/.config/tmux/tmux.conf',
      \ 'xinitrc'     : 'dotfiles/x-window/.config/X11/xinitrc',
      \ 'xmobar'      : 'dotfiles/x-window/.config/xmobar/xmobarrc',
      \ 'xmonad'      : 'dotfiles/x-window/.config/xmonad/xmonad.hs',
      \ 'xprofile'    : 'dotfiles/x-window/.config/X11/xprofile',
      \ 'wiki'        : 'dotfiles/nvim/.local/share/nvim/wiki/index.md',
      \ 'zshalias'    : 'dotfiles/shell/.config/zsh/lib/my_aliases.zsh',
      \ 'zshbindings' : 'dotfiles/shell/.config/zsh/lib/key-bindings.zsh',
      \ 'zshenv'      : 'dotfiles/shell/.zshenv',
      \ 'zshfunctions': 'dotfiles/shell/.config/zsh/lib/my_functions.zsh',
      \ 'zshrc'       : 'dotfiles/shell/.config/zsh/.zshrc',
      \ }

command! NvimRc :FreqCfgs nvim
nnoremap <silent> <Leader>ev :<C-U>FreqCfgs  nvim<CR>

command! Wiki   :FreqCfgs! wiki
nnoremap <silent> <Leader>vw :<C-U>FreqCfgs! wiki<CR>
