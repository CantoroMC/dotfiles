function! virgilio#channels#qfl#create(qflname,content)
  cexpr a:content
  call setqflist([], 'r', {'title': a:qflname})
  copen
endfunction
