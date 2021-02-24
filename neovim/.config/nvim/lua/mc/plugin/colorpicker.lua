local plugin_settings = {
  active     = true,
  light_time = { 7,14 },
  themes     = {
    [ "dark" ]  = {
      [ "slate" ] = 'base16_shell',
      [ "murphy" ] = 'dark',
    },
    [ "light" ] = {
      [ "morning" ] = 'light',
      [ "zellner" ] = 'base16_google',
    }
  },
  theme = { [ "murphy" ] = 'dark' },
}

local M = {}

function M.list(bg)
  return vim.tbl_keys(plugin_settings.themes[bg])
end

function M.choose(...)
  local args = {...}
  local bgs = { 'dark', 'light' }

  local background
  if #args >= 1 and #args <= 2 then
    background = args[1]
  elseif #args == 0 then
    local hour = tonumber(os.date('%H'))
    local bg_choose =
      (hour >= plugin_settings.light_time[1] and hour < plugin_settings.light_time[2]) and
        2 or 1
    background = bgs[bg_choose]
  else
    return
  end

  local theme
  local pairs = plugin_settings.themes[background]
  if #args == 2 then
    theme = args[#args]
  elseif #args <= 1 then
    local themes = vim.tbl_keys(pairs)
    math.randomseed(os.time())
    theme = themes[math.random(1,#themes)]
  end
  local airline = pairs[theme]

  vim.g.airline_theme = airline
  vim.o.background = background
  vim.cmd('colorscheme '..theme)
end

function M.setup(user_settings)
  if user_settings then
    plugin_settings = vim.tbl_extend("force", plugin_settings, user_settings)
  end

  if plugin_settings.active then
    require'mc.plugin.colorpicker'.choose()
  else
    vim.g.airline_theme = table.concat(vim.tbl_values(plugin_settings.theme))
    vim.cmd('colorscheme ' .. table.concat(vim.tbl_keys(plugin_settings.theme)))
  end
end

return M
