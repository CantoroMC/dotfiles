-- Weather module for weather.nvim plugin
local win, buf
local M = {}

local function create_window()
  local width      = vim.api.nvim_get_option("columns")
  local height     = vim.api.nvim_get_option("lines")
  local win_height = math.ceil(height * 0.3 - 8)
  local win_width  = math.ceil(width * 0.3 - 6)
  local x_pos      = 1
  local y_pos      = width - win_width

  local win_opts = {
    style    = "minimal",
    relative = "editor",
    width    = win_width,
    height   = win_height,
    row      = x_pos,
    col      = y_pos,
    border   = "single",
  }

  buf = vim.api.nvim_create_buf(false, true)
  win = vim.api.nvim_open_win(buf, true, win_opts)

  vim.api.nvim_buf_set_keymap(
    buf, "n", "q",
    ":lua require('mc.plugin.weather').close_window()<cr>",
    {noremap = true, silent = true})
  vim.api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')

  vim.api.nvim_win_set_option(win, 'winhl', 'Normal:Question')
  -- vim.api.nvim_win_set_option(win, "winblend", 80)
end

function M.show_weather(city)
  create_window()

  local city_param
  if vim.g.weather_city ~= nil then
    city_param = vim.g.weather_city
  elseif city ~= nil then
    city_param = city
  else
    city_param = ""
  end

  local command = string.format("curl https://wttr.in/%s'?'0", city_param)
  vim.api.nvim_call_function("termopen", {command})
end

function M.close_window()
  vim.api.nvim_win_close(win, true)
end

return M
