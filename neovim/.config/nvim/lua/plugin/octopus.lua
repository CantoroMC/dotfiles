local limbs = function()
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

local find_configured = function()
  local lua_dir = string.format(
    '%s/lua/plugin/config/',
    vim.fn.stdpath('config')
  )
  local vim_dir = string.format(
    '%s/plugin.d/',
    vim.fn.stdpath('config')
  )

  local lua_configs,vim_configs = {}, {}
  local iL,iV = 1,1

  for _, plugged  in ipairs(limbs()) do
    local lua_config = string.format(
      '%s%s.lua',
      lua_dir,plugged
    )
    local vim_config = string.format(
      '%s%s.vim',
      vim_dir,plugged
    )

    if vim.fn.filereadable(lua_config) == 1 then
      lua_configs[iL] = vim.fn.fnamemodify(lua_config,':t:r')
      iL = iL + 1
    end
    if vim.fn.filereadable(vim_config) == 1 then
      vim_configs[iV] = vim_config
      iV = iV + 1
    end
  end
  return lua_configs, vim_configs
end

local digest = function()
  local lua_configs, vim_configs = find_configured()

  for i = 1, #lua_configs, 1 do
    require(string.format(
      'plugin.config.%s',
      lua_configs[i])
    )
  end

  for i = 1, #vim_configs, 1 do
    vim.cmd('source ' .. vim_configs[i])
  end
end

local excrete = function()
  local lua_configs, vim_configs = find_configured()
  print(
    "Vim:\n" ..
    table.concat(
      vim.tbl_map(
        function(c) return vim.fn.fnamemodify(c, ':t:r') end,
        vim_configs
        ) , "\n") ..
    "\nLua:\n" ..
    table.concat(lua_configs, "\n")
  )
end

local ingest = function()
  local lua_configs, vim_configs = find_configured()
  return {
    vim.fn.fnamemodify(vim_configs, ':t:r'),
    lua_configs 
  }
end

return {
  limbs = limbs,
  digest = digest,
  excrete = excrete,
  ingest = ingest
}
