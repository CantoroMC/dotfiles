local plugin_settings = {
  active     = true,
  light_time = { 7,14 },
  themes     = {
    [ "dark" ]  = { 'slate', 'murphy' },
    [ "light" ] = { 'morning', 'zellner' }
  },
  theme = 'murphy',
}

local M = {}

math.randomseed(os.time())

function M.choose(...)
  local args = {...}
  local bgs = { 'dark', 'light' }

  local background
  if #args >= 1 and #args <= 2 then
    background = #args[1]
  elseif #args == 0 then
    local hour = tonumber(os.date('%H'))
    local bg_choose =
      (hour >= plugin_settings.light_time[1] and hour < plugin_settings.light_time[2]) and
        2 or 1
    background = bgs[bg_choose]
  else
    return
  end

  local themes = plugin_settings.themes[background]
  local theme
  if #args == 2 then
    theme = args[#args]
  elseif #args <= 1 then
    theme = themes[math.random(1,#themes)]
  end

  vim.o.background = background
  vim.cmd('colorscheme '..theme)
end

function M.setup(user_settings)
  if user_settings then
    plugin_settings = vim.tbl_deep_extend("force", plugin_settings, user_settings)
  end

  if plugin_settings.active then
    require'mc.plugin.colorpicker'.choose()
  else
    vim.cmd('colorscheme ' .. plugin_settings.theme)
  end
end

return M
