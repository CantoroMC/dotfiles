-- Pretty printer
PP = function(v)
  print(vim.inspect(v))
  return v
end

-- Web Devicons wrapper
function _G.webDevIcons(path)
  local filename = vim.fn.fnamemodify(path, ':t')
  local extension = vim.fn.fnamemodify(path, ':e')
  return require'nvim-web-devicons'.get_icon(filename, extension, { default = true })
end
