local packed = function()
  local directory = string.format('%s/site/pack/packer', vim.fn.stdpath('data'))
  local globpath  = string.format('%s/opt/,%s/start',    directory, directory)

  local plugins = {}
  for _, path in ipairs(vim.fn.globpath(globpath, '*', true, true)) do
    plugins[_] = vim.fn.fnamemodify(path, ':t')
  end

  return plugins
end

local find_configured = function()
  local directory = string.format('%s/lua/plugin/configs/',vim.fn.stdpath('config'))

  local configs = {}
  local i = 1
  for _, plugged  in ipairs(packed()) do
    local config = string.format('%s%s.lua', directory, plugged)
    if vim.fn.filereadable(config) == 1 then
      configs[i] = vim.fn.fnamemodify(config,':t:r')
      i = i + 1
    end
  end
  return configs
end

local configs = find_configured()
for i = 1, #configs, 1 do
  require(string.format('plugin.configs.%s',configs[i]))
end
