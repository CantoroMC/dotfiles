local vim = vim

local M = {}

function M.treesitter()
  return vim.fn['nvim_treesitter#statusline']({
    indicator_size = 100,
    type_patterns  = { 'class', 'function', 'method' },
    transform_fn   =
      function(line)
        return line:gsub('%s*[%[%(%{]*%s*$', '')
      end,
    separator      = ' -> '
  })
end

return M
