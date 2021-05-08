local bgs = { 'dark', 'light' }
local M = {}
local plug_confs = {
  active     = true,
  light_time = { 6, 7 },
  themes     = {
    [ "dark" ]  = {
      [ "ayu" ]        = 'ayu',
      [ "PaperColor" ] = 'papercolor_dark',
      [ "sitruuna" ]   = 'codedark',
      -- [ "gruvbox" ]    = 'gruvbox_material',
      -- [ "jellybeans" ] = 'codedark',
      -- [ "badwolf" ]    = 'codedark',
      -- [ "srcery" ]     = 'codedark',
    },
    [ "light" ] = {
      [ "mayansmoke" ] = 'iceberg_light',
      [ "PaperColor" ] = 'papercolor_light',
    }
  },
  theme = { [ "ayu" ] = 'ayu'},
  add_highlight = {
    ["Comment"] = 'gui=italic',
  }
}

local function bg_color()
  local hour = tonumber(os.date('%H'))
  return
    (hour >= plug_confs.light_time[1] and hour < plug_confs.light_time[2])
      and 2 or 1
end

function M.list(bg)
  return vim.tbl_keys(plug_confs.themes[bg])
end

function M.paint(...)
  local args = {...}

  local bg
  if #args >= 1 and #args <= 2 then
    bg = args[1]
  elseif #args == 0 then
    bg = bgs[bg_color()]
  else
    return
  end

  local theme
  if #args == 2 then
    theme = args[#args]
  elseif #args <= 1 then
    local themes = vim.tbl_keys(plug_confs.themes[bg])
    math.randomseed(os.time())
    theme = themes[math.random(1,#themes)]
  end

  vim.o.background = bg
  vim.cmd('colorscheme '..theme)
end

function M.lualine()
  if plug_confs.active then
    return plug_confs.themes[vim.o.background][vim.g.colors_name]
  else
    return table.concat(vim.tbl_values(plug_confs.theme))
  end
end

function M.setup(usr_conf)
  if usr_conf then
    plug_confs = vim.tbl_extend("force", plug_confs, usr_conf)
  end

  if plug_confs.active then
    require'mc.plugin.picasso.brush'.paint()
  else
    vim.o.background = bg_color() == 2 and 'light' or 'dark'
    require'mc.plugin.picasso.brush'.paint(
      bg_color() == 2 and 'light' or 'dark',
      table.concat(vim.tbl_keys(plug_confs.theme))
    )
  end

  for k,v in pairs(plug_confs.add_highlight) do
    vim.cmd('highlight ' .. k .. ' ' .. v)
  end
end

return M
