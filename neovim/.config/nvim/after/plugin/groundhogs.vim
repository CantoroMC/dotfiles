command! -count=1 -complete=shellcmd -nargs=*
      \ GroundExec   lua require'groundhogs'.exec(<q-args>, <count>)
command! -count=1 
      \ GroundToggle lua require'groundhogs'.toggle(<count>)
