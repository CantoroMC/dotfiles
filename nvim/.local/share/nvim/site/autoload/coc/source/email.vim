function! coc#source#email#init() abort
  return {
        \ 'priority': 1,
        \ 'shortcut': '@',
        \ 'triggerCharacters': ['@']
        \}
endfunction

function! coc#source#email#complete(opt, cb) abort
  let items = s:AbookMails()
  call a:cb(items)
endfunction

function s:AbookMails() abort
  let mails = systemlist('grep email $ADDRESSES')
  let mails = map(
        \ mails,
        \ 'split(substitute(v:val, "email=", "", "ge"), ",")'
        \ )
  let emails = []
  for i in mails
    for j in i
      let emails = extend(emails, [j])
    endfor
  endfor
  return emails
endfunction
