local vim,fn, cmd = vim, vim.fn, vim.cmd
local b, bo = vim.b, vim.bo
local fmt = string.format



local M = {}

local enabled = false
local cache = ''
local options = {
  c_langs = {'arduino', 'c', 'cpp', 'cuda', 'go', 'javascript', 'ld', 'php'},
  max_lines = 5000,
}



local function search(prefix, pattern)
  local line = fn.search(pattern, 'nw')
  if line == 0 then
    return ''
  end
  return fmt('[%s:%d]', prefix, line)
end

local function set_cache_autocmds(augroup)
  cmd(fmt('augroup %s', augroup))
  cmd('autocmd!')
  cmd(fmt('autocmd CursorHold,BufWritePost * unlet! b:%s', augroup))
  cmd('augroup END')
end


local function check_trailing()
  return search('trail', [[\s$]])
end

local function check_mix_indent()
  local tst = [[(^\t* +\t\s*\S)]]
  local tls = fmt([[(^\t+ {%d,}\S)]], bo.tabstop)
  local pattern = fmt([[\v%s|%s]], tst, tls)
  return search('mix-ind', pattern)
end

local function check_mix_indent_file()
  local head_spc = [[\v(^ +)]]
  if vim.tbl_contains(options.c_langs, bo.filetype) then
    head_spc = [[\v(^ +\*@!)]]
  end
  local indent_tabs = fn.search([[\v(^\t+)]], 'nw')
  local indent_spc = fn.search(head_spc, 'nw')
  if indent_tabs == 0 or indent_spc == 0 then
    return ''
  end
  return fmt('[mix-ind-f:%d,%d]', indent_spc, indent_tabs)
end

local function check_conflict()
  local annotation = [[\%([0-9A-Za-z_.:]\+\)\?]]
  local raw_pattern = [[^\%%(\%%(<\{7} %s\)\|\%%(=\{7\}\)\|\%%(>\{7\} %s\)\)$]]
  if bo.filetype == 'rst' then
    raw_pattern = [[^\%%(\%%(<\{7} %s\)\|\%%(>\{7\} %s\)\)$]]
  end
  local pattern = fmt(raw_pattern, annotation, annotation)
  return search('confl', pattern)
end



function M.whitespaces()
  if not enabled then
    set_cache_autocmds('lualine_whitespace')
    enabled = true
  end
  if bo.readonly or not bo.modifiable then
    return ''
  end
  if fn.line('$') > options.max_lines then
    return ''
  end
  if b.lualine_whitespace then
    return cache
  end
  b.lualine_whitespace = true
  cache = table.concat({
    check_trailing(),
    check_mix_indent(),
    check_mix_indent_file(),
    check_conflict(),
  })
  return cache
end

return M
