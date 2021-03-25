-- Return a list of all the packed plugins
local head = function()
  local directory = string.format(
    '%s/site/pack/packer',
    vim.fn.stdpath('data')
  )
  local globpath  = string.format(
    '%s/opt/,%s/start',
    directory, directory
  )

  local plugins = {}
  for _, path in ipairs(vim.fn.globpath(globpath, '*', true, true)) do
    plugins[_] = vim.fn.fnamemodify(path, ':t')
  end

  return plugins
end

-- Return a list of configuration file in mc.plugin.config that have a
-- corresponding installed plugin
local limbs = function()
  local lua_dir = string.format(
    '%s/lua/mc/plugin/config/',
    vim.fn.stdpath('config')
  )
  local vim_dir = string.format(
    '%s/plugin.d/',
    vim.fn.stdpath('config')
  )

  local lua_configs, vim_configs = {}, {}
  local iL, iV = 1, 1

  for _, plugged  in ipairs(head()) do
    local lua_config = string.format(
      '%s%s.lua',
      lua_dir, plugged
    )
    local vim_config = string.format(
      '%s%s.vim',
      vim_dir, plugged
    )
    if vim.fn.filereadable(lua_config) == 1 then
      lua_configs[iL] = vim.fn.fnamemodify(lua_config,':t:r')
      iL = iL + 1
    end
    if vim.fn.filereadable(vim_config) == 1 then
      vim_configs[iV] = vim.fn.fnamemodify(vim_config,':t:r')
      iV = iV + 1
    end
  end
  return lua_configs, vim_configs
end

-- Require the configuration
local digest = function()
  local lua_configs, vim_configs = limbs()
  for i = 1, #lua_configs, 1 do
    require(string.format(
      'mc.plugin.config.%s',
      lua_configs[i])
    )
  end
  for i = 1, #vim_configs, 1 do
    vim.cmd(
      'source ' ..
      vim.fn.stdpath('config') .. '/plugin.d/' .. vim_configs[i] .. '.vim'
    )
  end
end

-- Print the list of lua 's plugin configuration
local excrete = function()
  local lua_configs,vim_configs = limbs()
  local cexpr =
    "Lua Required:\n" ..
    "=============\n" ..
    table.concat(lua_configs, "\n") ..
    "\n\nVim Required:\n" ..
    "=============\n" ..
    table.concat(vim_configs, "\n")
  vim.cmd('cexpr ' .. vim.fn.string(cexpr))
  vim.fn.setqflist({}, 'a', {["title"] = 'Octolua'})
  vim.cmd 'copen'
end

return {
  head = head,
  limbs = limbs,
  digest = digest,
  excrete = excrete,
}
