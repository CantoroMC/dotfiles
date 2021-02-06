PP = function(v)
  print(vim.inspect(v))
  return v
end

Cmd  = vim.cmd
Fn   = vim.fn
Map  = vim.api.nvim_set_keymap
BMap = vim.api.nvim_buf_set_keymap

function Set_opt(opts_table)
  local scopes = { o = vim.o, b = vim.bo, w = vim.wo }
  for k,v in pairs(opts_table) do
    local scope = v[1]
    local value = v[2]
    scopes[scope][k] = value
  end
end

function Set_var(vars_table)
  local scopes = { g = vim.g, b = vim.b, w = vim.w, t = vim.t, v = vim.v }
  for k,v in pairs(vars_table) do
    local scope = v[1]
    local value = v[2]
    scopes[scope][k] = value
  end
end

function Set_map(maps_table)
  for _,v in pairs(maps_table) do
    local mode = v[1]
    local lhs  = v[2]
    local rhs  = v[3]
    local args = v[4]
    Map(mode, lhs, rhs, args)
  end
end
