-----------------------------------------------------------------------------
-- Neovim Plugin written in Lua that allow to spawn multiple terminals
-- with ease and with "REPL" capabilities.
-- Author: Marco Cantoro
-----------------------------------------------------------------------------

local api = vim.api
local fn = vim.fn
local shadings = require'mc.plugin.groundhogs.shadings'

local groundhogs_ft = 'GroundHog'
local plugin_settings = {
  size            = 18,
  remember_size   = true,
  direction       = 'horizontal',
  shade_terminals = true,
  start_in_insert = false,
}

local groundhogs = {}
local remember   = {}
local prev_win

local M = {}



--- Define vim autogroups
  -- convert a table of augroup definitions in effective Vim autogroups
  -- @param augs_tbl table<string,table>
local function create_augs(augs_tbl)
  for name, def in pairs(augs_tbl) do
    vim.cmd('augroup ' .. name)
    vim.cmd 'autocmd!'
    for _, args in pairs(def) do
      local autocmd = table.concat(vim.tbl_flatten { 'autocmd', args }, ' ')
      vim.cmd(autocmd)
    end
    vim.cmd 'augroup END'
  end
end


--- Instantiate an empty groundhog table
  -- Retrieve the number of already existing groundhogs and then create
  -- an empty groundhogs with the next number
local function create_groundhog()
  local population = #groundhogs
  local num = population == 0 and 1 or population + 1
  return {
    win_id = -1,
    job_id = -1,
    bufnr  = -1,
    dir    = fn.getcwd(),
    number = num
  }
end

--- Check if there is already a groundhog with number
  -- equal to the supplied argument, if so it is returned
  -- otherwise the next groundhog will born.
  -- @param num number representing the sought groundhog
local function catch_groundhog(num)
  return groundhogs[num] or create_groundhog()
end

local function update_prev_win(win_id)
  local curr_win = api.nvim_get_current_win()
  if win_id ~= curr_win then
    prev_win = curr_win
  end
end

local function check_and_go_to_winid(win_id)
  return fn.win_gotoid(win_id) > 0
end

--- Check if there are any already existing groundhogs
  -- If there are it will return true and a list of existing groundhogs
  -- If there are not it will return false and nil
local function visible_groundhogs()
  comparator = function(buf)
    return vim.bo[buf].filetype == groundhogs_ft
  end

  local wins = api.nvim_list_wins()
  local is_open = false
  local groundhog_wins = {}
  for _, win in pairs(wins) do
    local buf = api.nvim_win_get_buf(win)
    if comparator(buf) then
      is_open = true
      table.insert(groundhog_wins, win)
    end
  end
  return is_open, groundhog_wins
end


--- Get the size of the split.
  -- Order of priority is as follows:
  --   1. The size argument is a valid number > 0
  --   2. There is window dimensions from previous open state and are required
  --   3. Default preference size
  -- @param size number passed to the groundhog toggling
local function get_size(size)
  local valid_size = size ~= nil and size > 0

  if not plugin_settings.remember_size then
    return valid_size and size or plugin_settings.size
  end

  local psize = plugin_settings.direction == 'horizontal'
    and remember.height or remember.width
  return valid_size and size or psize or plugin_settings.size
end

local function resize(size)
  local cmd = plugin_settings.direction == 'vertical' and
    'vertical resize' or 'resize'
  vim.cmd(cmd .. size)
end

--- Open a groundhog according the existence of
  -- already existing groundhogs
  -- @param size the size of the groundhog population
local function open_split(size)
  size = get_size(size)

  local has_open, groundhog_ids = visible_groundhogs()

  h_push = vim.o.splitright and 'wincmd L' or 'wincmd H'
  v_push = vim.o.splitbelow and 'wincmd J' or 'wincmd K'
  local commands = plugin_settings.direction == 'horizontal' and
    { 'vsplit', 'split',  v_push } or
    { 'split',  'vsplit', h_push }

  if has_open then
    -- we need to be in the terminal window most recently opened
    -- in order to split to the right of it
    fn.win_gotoid(groundhog_ids[#groundhog_ids])
    vim.cmd(commands[1])
  else
    vim.cmd(commands[2])
    vim.cmd(commands[3])
  end

  resize(size)
end

local function set_groundhog_win_opts(groundhog)
  win_id = groundhog.win_id
  vim.wo[win_id].winfixheight   = true
  vim.wo[win_id].winfixwidth    = true
  vim.wo[win_id].spell          = false
  vim.wo[win_id].foldenable     = false
  vim.wo[win_id].number         = false
  vim.wo[win_id].relativenumber = false
  vim.wo[win_id].signcolumn     = 'no'
  if plugin_settings.start_in_insert then
    vim.cmd("startinsert!")
  end
end

local function set_groundhog_buf_opts(groundhog)
  bufnr = groundhog.bufnr
  num   = groundhog.number

  vim.bo[bufnr].filetype        = groundhogs_ft
  vim.bo[bufnr].buflisted       = false
  vim.bo[bufnr].swapfile        = false
  vim.bo[bufnr].modified        = false
  -- a buffer variable to set the status line name
  api.nvim_buf_set_var(bufnr, "groundhogs_number", num)

  if plugin_settings.shade_terminals then
    shadings.set_highlights(-30)
    shadings.shade_groundhogs()
  end
end

function save_groundhogs_size()
  remember.width = vim.fn.winwidth(0)
  remember.height = vim.fn.winheight(0)
end

local function find_windows_by_bufnr(bufnr)
  return fn.win_findbuf(bufnr)
end



function close(num)
  local groundhog = catch_groundhog(num)

  update_prev_win(groundhog.win_id) -- Is it necessary??

  if check_and_go_to_winid(groundhog.win_id) then
    if plugin_settings.remember_size then
      save_groundhogs_size()
    end
    vim.cmd("hide")

    if api.nvim_win_is_valid(origin_win) then
      api.nvim_set_current_win(origin_win)
    else
      origin_win = nil
    end
  else
    if num then
      vim.cmd(string.format(
        'echoerr "Failed to close window: %d does not exist"', num)
      )
    else
      vim.cmd('echoerr "Failed to close window: invalid groundhog number"')
    end
  end
end

function open(num, size)
  vim.validate {
    num  = { num,  'number' },
    size = { size, 'number', true }
  }

  local groundhog = catch_groundhog(num)
  prev_win = api.nvim_get_current_win()

  open_split(size)
  if vim.fn.bufexists(groundhog.bufnr) == 0 then
    groundhog.win_id = fn.win_getid()
    groundhog.bufnr  = api.nvim_create_buf(false, false)

    api.nvim_set_current_buf(groundhog.bufnr)
    api.nvim_win_set_buf(groundhog.window, groundhog.bufnr)

    local cmd = vim.o.shell .. ";#" .. groundhogs_ft .. "#" .. num
    groundhog.job_id = fn.termopen(cmd, {detach = true})
    groundhogs[num] = groundhog
    set_groundhog_buf_opts(groundhog)

    local aucmds = {
      { "TermClose", string.format("<buffer=%d>", groundhog.bufnr),
        string.format("lua require'mc.plugin.groundhogs'.free(%d)", num)
      }
    }
    create_augs({["GroundHogs" .. groundhog.bufnr] = aucmds})
  else
    vim.cmd('keepalt buffer ' .. groundhog.bufnr)
    groundhog.win_id = fn.win_getid()
  end
  set_groundhog_win_opts(groundhog)
end

local function smart_toggle(size)
  local is_one_open = visible_groundhogs()
  if not is_one_open then
    open(1, size)
  else
    local target = #groundhogs
    for i = #groundhogs, 1, -1 do
      local groundhog = groundhogs[i]
      if not groundhog then
        vim.cmd(string.format(
          'echomsg "Groundhog does not exist %s"',
          vim.inspect(groundhog)
          )
        )
        break
      end

      local wins = find_windows_by_bufnr(groundhog.bufnr)
      if #wins > 0 then
        target = i
        break
      end
    end
    close(target)
  end
end


local function toggle_nth(num, size)
  local groundhog = catch_groundhog(num)

  update_prev_win(groundhog.win_id)

  if check_and_go_to_winid(groundhog.win_id) then
    close(num)
  else
    open(num, size)
  end
end



function M.close_if_last_is_groundhog()
  local buf = api.nvim_get_current_buf()
  local only_groundhogs = fn.winnr("$") == 1
  if only_groundhogs and vim.bo[buf].filetype == groundhogs_ft then
    vim.cmd("quit")
  end
end

function M.free(num)
  if groundhogs[num] then
    groundhogs[num] = nil
  end
end



function M.exec(cmd, num, size)
  vim.validate {
    cmd  = { cmd,  'string' },
    num  = { num,  'number' },
    size = { size, 'number', true }
  }

  num = num < 1 and 1 or num
  local groundhog = catch_groundhog(num)
  if not check_and_go_to_winid(groundhog.win_id) then
    open(num, size)
  end

  groundhog = catch_groundhog(num)
  fn.chansend(groundhog.job_id, cmd .. "\n")
  vim.cmd('wincmd p')
end

--- Toggle the terminal groundhogs
  -- If a num, except 1, is provided we operate the specific terminal buffer.
  -- If the num is 1 or not provided:
  -- 1) If there is someone open with close it, starting from those with
  --    higher number
  -- 2) If there is not an open terminal buffer we open it starting with the
  --    lower number
  -- @param size number
function M.toggle(num, size)
  vim.validate {
    num  = {num,  'number', true},
    size = {size, 'number', true}
  }

  if num > 1 then
    toggle_nth(num, size)
  else
    smart_toggle(size)
  end
end

--- Lua Plugin initialization
  -- Store user settings and initialize Vim autocommands.
  -- @param user_settings table of user settings
function M.setup(user_settings)
  if user_settings then
    plugin_settings = vim.tbl_deep_extend("force", plugin_settings, user_settings)
  end

  local aucmds = {
    { 'BufEnter', '*',
      "++nested", "lua require'mc.plugin.groundhogs'.close_if_last_is_groundhog()"
    },
  }
  create_augs({GroundHogs = aucmds})
end

function M.introspect()
  print("All terminals: " .. vim.inspect(groundhogs))
end

return M
