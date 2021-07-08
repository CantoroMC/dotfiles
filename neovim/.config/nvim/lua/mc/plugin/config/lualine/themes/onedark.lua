local c = require'onedark.colors'

local onedark = {
  normal = {
    a = { fg = c.bg,      bg = c.green,  gui = 'bold' },
    b = { fg = c.fg,      bg = c.colorcolumn },
    c = { fg = c.comment, bg = c.bg,     gui = 'italic' },
  },
  insert = {
    a = { fg = c.black,   bg = c.purple, gui = 'bold' },
    b = { fg = c.fg,      bg = c.colorcolumn },
    c = { fg = c.comment, bg = c.bg,     gui = 'italic' },
  },
  replace = {
    a = { fg = c.black,   bg = c.cyan, gui = 'bold' },
    b = { fg = c.fg,      bg = c.colorcolumn },
    c = { fg = c.white,   bg = c.bg,     gui = 'italic' },
  },
  visual = {
    a = { fg = c.black,   bg = c.cyan,   gui = 'bold' },
    b = { fg = c.fg,      bg = c.colorcolumn },
    c = { fg = c.comment, bg = c.bg,     gui = 'italic' },
  },
  inactive = {
    a = { fg = c.black,   bg = c.gray,   gui = 'bold' },
    b = { fg = c.fg,      bg = c.colorcolumn },
    c = { fg = c.comment, bg = c.bg,     gui = 'italic' },
  }
}

return onedark
