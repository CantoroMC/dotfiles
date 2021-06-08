" Section: Commands

command! -buffer Dbstep     SlimuxShellRun dbstep
command! -buffer DbstepIn   SlimuxShellRun dbstep in
command! -buffer Dbcont     SlimuxShellRun dbcont
command! -buffer DbstepOut  SlimuxShellRun dbstep out
command! -buffer Dbquit     SlimuxShellRun dbquit
command! -buffer Dbstatus   SlimuxShellRun dbstatus
command! -buffer DbstopHere :execute 'SlimuxShellRun dbstop in '.expand('%').' at '. line('.')
command! -buffer -nargs=1 -complete=file
      \ DbclearAll SlimuxShellRun dbclear all in <args>
command! -buffer -nargs=1 Dbstop
      \ :execute 'SlimuxShellRun dbstop in '.expand('%').' at '. <args>

" Section: Mappings
nnoremap <silent> <buffer> <C-l><C-s> :<C-U>Dbstep<CR>
nnoremap <silent> <buffer> <C-l><C-i> :<C-U>DbstepIn<CR>
nnoremap <silent> <buffer> <C-l><C-h> :<C-U>DbstopHere<CR>
nnoremap <silent> <buffer> <C-l><C-c> :<C-U>Dbcont<CR>
