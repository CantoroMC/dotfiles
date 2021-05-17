local fn = vim.fn
local fmt = string.format


local M = {}

function M.nr_lines()
  return [[☰ %3L]]
end

function M.file_size()
  local file = fn.expand('%:p')
  if string.len(file) == 0 then return '' end

  local size = fn.getfsize(file)
  if size <= 0 then return '' end
  local sufixes = {'b', 'k', 'm', 'g'}

  local i = 1
  while size > 1024 do
    size = size / 1024
    i = i + 1
  end
  return fmt('[%.1f%s]', size, sufixes[i])
end

return M
