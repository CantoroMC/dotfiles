command! -count=1 -complete=shellcmd -nargs=*
      \ GroundExec   lua require'mc.plugin.groundhogs'.exec(<q-args>, <count>)
command! -count=1
      \ GroundToggle lua require'mc.plugin.groundhogs'.toggle(<count>)

nnoremap <silent> <C-\>
      \ :<C-U>execute v:count1.'GroundToggle'<CR>
inoremap <silent> <C-\>
      \ <Esc>:<C-U>execute v:count1.'GroundToggle'<CR>
