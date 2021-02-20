local fn = vim.fn
local M = {}

local function get_file_icon()
  local ok,devicons = pcall(require,'nvim-web-devicons')
  if not ok then
    print('No icon plugin found. Please install \'kyazdani42/nvim-web-devicons\'')
    return ''
  end

  local icon, iconhl = devicons.get_icon(fn.expand('%:t'),fn.expand('%:e'))
  if icon ~= nil then
    return fn.synIDattr(vim.fn.hlID(iconhl), 'fg')
  end

  return icon .. ' '
end

local function filter(l,p)
  f = {}
  for _, v in ipairs(l) do
    if p(v) then f[#f+1] = v end
  end
  return f
end

local function tab_label(n)
  local winnr = fn.tabpagewinnr(n)
  local bufs = fn.tabpagebuflist(n)
  local cur_buf = fn.bufname(bufs[winnr])

  local mod_bufs = {}
  for _,v in ipairs(bufs) do
    if (vim.bo[v].modified == false) then
      mod_bufs[#mod_bufs+1] = v
    end
  end

  local icon = get_file_icon()
  local name = cur_buf == '' and
    '[No Name]' or fn.pathshorten(fn.fnamemodify(cur_buf, ':p:~:.'))
  local modifier = #mod_bufs > 0 and #mod_bufs .. '+/' or ''

  return string.format('%s %s [%s%s]', icon, name, modifier, #bufs)
end

local function buf_label(b)
  local name = (fn.bufname(b) == '') and
    '[No Name]' or fn.pathshorten(fn.fnamemodify(fn.bufname(b), ':p:~:.'))
  local modifier = vim.bo[b].modified == true and '[+]' or ''

  return string.format(' %s%s ', name, modifier)
end

function M.bufferline()
  local s = ''

  s = s .. ' Buffers: '

  local cur_buf = fn.winbufnr(0)
  local bufs = {}
  for v = 1, fn.bufnr('$'),1 do
    if (vim.fn.buflisted(v) == 1 and vim.bo[v].buftype ~= 'quickfix') then
      bufs[#bufs+1] = v
    end
  end

  for _,buf in ipairs(bufs) do
    s = s .. (buf == cur_buf
      and '%#BufTabLineCurrent#' or (fn.bufwinnr(buf) > 0
        and '%#BufTabLineActive#' or '%#BufTabLineHidden#'))
    s = s .. buf_label(buf)
    s = s .. '%#TabLineFill#'
  end

  s = s .. '%#TabLineFill#%='

  if fn.tabpagenr('$') > 1 then
    s = s .. 'Tabs: '
    for tab = 1, fn.tabpagenr('$'),1 do
      s = s .. (tab == fn.tabpagenr() and '%#TabLineSel#' or '%#TabLine#')
      s = s .. '%' .. tab .. 'T'
      s = s .. tab_label(tab)
    end
    s = s .. '%#TabLineFill#%999X[]'
  end

  return s
end

return M
