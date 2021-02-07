--[[ Format Table
  |  t | Auto-wrap text using textwidth                                       |
  |  c | Auto-wrap comments using textwidth, inserting the current            |
  |        comment leader automatically                                       |
  |  r | Automatically insert the current comment leader after                |
  |        hitting <Enter> in Insert mode                                     |
  |  o | Automatically insert the current comment leader after                |
  |        hitting o in Insert mode                                           |
  |  q | Allow formatting of comments with "gq".                              |
  |  w | Trailing white space indicates a paragraph continues in the next line| .
  |        A line that ends in a non-white character ends a paragraph.        |
  |  a | Automatic formatting of paragraphs.                                  |
  |  n | When formatting text, recognize numbered lists                       |
  |  2 | When formatting text, use the indent of the second line of a         |
  |        paragraph for the rest of the paragraph, instead of the indent of  |
  |        the first line.                                                    |
  |  v | Vi-compatible auto-wrapping in insert mode: Only break a line at a   |
  |        blank that you have entered during the current insert command.     |
  |  l | Long lines are not broken in insert mode                             |
  |  m | Also break at a multi-byte character above 255.                      |
  |  B | When joining lines, don't insert a space between two                 |
  |        multi-byte characters                                              |
  |  1 | Don't break a line after a one-letter word.  It's broken before it   |
  |        instead.                                                           |
  |  j | Where it make sense, remove a comment leader when joining lines      |
--]]

vim.cmd('augroup user_formatoptions')
  vim.cmd('autocmd!')
  vim.cmd(FtAuCmd( { '*' }, { 'formatoptions=cqjrnBl' }))
vim.cmd('augroup END')

vim.cmd('augroup comment_strings')
  vim.cmd('autocmd!')
  vim.cmd(FtAuCmd( { 'c', 'cpp', 'cs' }, { [[commentstring=// %s]] }))
  vim.cmd(FtAuCmd( { 'desktop' },        { [[commentstring=# %s]] }))
vim.cmd('augroup END')

vim.cmd('augroup keywordprg_filetype')
  vim.cmd('autocmd!')
  vim.cmd(FtAuCmd( { 'ruby' },    { [[keywordprg=ri --format=markdown]] }))
  vim.cmd(FtAuCmd( { 'haskell' }, { [[keywordprg=hoogle\ -q\ --info]] }))
vim.cmd('augroup END')
