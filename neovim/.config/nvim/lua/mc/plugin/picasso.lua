-- Colorscheme Configurations ================================================
-- Ayu
vim.g.ayu_mirage          = false
vim.g.ayu_disable_bg      = false
vim.g.ayu_borders         = false
vim.g.ayu_italic_comments = true
vim.g.ayu_italic_strings  = true
vim.g.ayu_contrast        = true
vim.g.ayu_contrast_amount = -10
-- Badwolf
vim.g.badwolf_darkgutter          = true  -- gutters are rendered darker (bool)
vim.g.badwolf_tabline             = 0     -- Tab line background (0..3) the lower the darker
vim.g.badwolf_html_link_underline = true  -- Underline text inside `a` tags in HTML (bool)
vim.g.badwolf_css_props_highlight = true  -- Highlight CSS properties (bool)
-- Gruvbox
vim.g.gruvbox_italic               = 1
vim.g.gruvbox_bold                 = 1
vim.g.gruvbox_underline            = 1
vim.g.gruvbox_undercurl            = 1
vim.g.gruvbox_contrast_light       = 'hard'
vim.g.gruvbox_contrast_dark        = 'hard'
vim.g.gruvbox_italicize_comments   = 1
vim.g.gruvbox_italicize_strings    = 1
vim.g.gruvbox_invert_indent_guides = 1
vim.g.gruvbox_invert_tabline       = 1
vim.g.gruvbox_improves_strings     = 1
vim.g.gruvbox_improved_warnings    = 1
-- Material
vim.g.material_style           = 'darker'
vim.g.material_contrast        = true
vim.g.material_italic_comments = true
vim.g.material_borders         = true
-- PaperColor
vim.g.PaperColor_Theme_Options = {
  ["theme"] = {
    ["default.dark"] = {
      ["transparent_background"] = 0,
      ["allow_bold"]             = 1,
      ["allow_italic"]           = 1,
    },
    ["default.light"] = {
      ["transparent_background"] = 0,
      ["allow_bold"]             = 1,
      ["allow_italic"]           = 1,
    },
  },
  ["language"] = {
    ["python"] = { ["highlight_builtins"] = 1 },
    ["cpp"]    = { ["highlight_standard_library"] = 1 },
    ["c"]      = { ["highlight_builtins"] = 1 },
  },
}
-- Srcery
vim.g.srcery_italic    = 1
vim.g.srcery_bold      = 1
vim.g.srcery_underline = 1
vim.g.srcery_undercurl = 1


-- Picasso ===================================================================
local M = {}

local bgs = { 'dark', 'light' }
local plug_confs = {
  active     = false,
  light_time = { 6, 20 },
  themes     = {
    [ "dark" ]  = {
      [ "ayu" ]        = 'ayu',
      [ "material" ]   = 'material',
      -- [ "PaperColor" ] = 'papercolor_dark',
      -- [ "sitruuna" ]   = 'codedark',
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
  }
}

local function isLight()
  local hour = tonumber(os.date('%H'))
  return
    (hour >= plug_confs.light_time[1] and hour < plug_confs.light_time[2])
      and true or false
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
    bg = bgs[isLight() and 2 or 1]
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

  for k,v in pairs(plug_confs.add_highlight) do
    vim.cmd('highlight ' .. k .. ' ' .. v)
  end
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
    M.paint()
  else
    vim.o.background = isLight() and 'light' or 'dark'
    M.paint(
      isLight() and 'light' or 'dark',
      table.concat(vim.tbl_keys(plug_confs.theme))
    )
  end
end

return M
