let g:dispatch_no_maps = 1

augroup dispatch_configs
  autocmd BufReadPost *
        \ if getline(1) =~# '^#!' |
        \   let b:dispatch =
        \       matchstr(getline(1), '#!\%(/usr/bin/env \+\)\=\zs.*') . ' %' |
        \   let b:start = '-wait=always ' . b:dispatch |
        \ endif

  autocmd FileType html let b:dispatch = ':Browse'
  autocmd FileType tmux let b:dispatch = 'tmux source %:p:S'
  autocmd FileType perl let b:dispatch = 'perl -Wc %'
  autocmd FileType ruby
        \ if !exists('b:start') |
        \   let b:start = executable('pry') ? 'pry -r %:p:S' : 'irb -r %:p:S' |
        \ endif |
        \ if expand('%') =~# '_test\.rb$' |
        \   let b:dispatch = 'testrb %' |
        \ elseif expand('%') =~# '_spec\.rb$' |
        \   let b:dispatch = get(b:, 'dispatch', 'rspec %:s/$/\=exists("l#") ? ":".l# : ""/') |
        \ elseif join(getline(max([line('$')-8,1]), '$'), "\n") =~# '\$0\>' |
        \   let b:dispatch = 'ruby %' |
        \ elseif !exists('b:dispatch') |
        \   let b:dispatch = 'ruby -wc %' |
        \ endif

  autocmd FileType tex     let b:dispatch = 'latexmk -pdf --output-directory=%:h %'
  autocmd FileType haskell let b:dispatch = 'ghc --make %'
  autocmd FileType python  let b:dispatch = 'python -m py_compile %'
  autocmd FileType c       let b:dispatch = 'gcc -g -o %:r %'
  autocmd FileType cpp     let b:dispatch = 'g++ -g -o %:r %'
  autocmd FileType matlab  let b:dispatch = 'mlint -id %'
augroup END

nnoremap <silent> <C-c><C-c> :<C-u>Make<CR>
nnoremap <silent> <C-c><C-d> :<C-u>Dispatch!<CR>
