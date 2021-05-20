local vim = vim

local M = {}

function M.vista(vista_icon)
  local has_vista,vista_info = pcall(
    vim.api.nvim_buf_get_var, 0, 'vista_nearest_method_or_function'
  )
  if not has_vista then return end
  local icon = vista_icon or '襁'
  return icon .. vista_info
end

return M
