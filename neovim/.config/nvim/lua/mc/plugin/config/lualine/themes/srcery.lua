local colors = require'srcery.colors'

local srcery = {
  normal = {
    a = {fg = colors.bright_white, bg = colors.xgray4, gui = 'bold'  },
    b = {fg = colors.bright_white, bg = colors.xgray3                },
    c = {fg = colors.bright_white, bg = colors.xgray2, gui = 'italic'},
  },
  insert = {
    a = {fg = colors.black,        bg = colors.bright_white,  gui = 'bold'  },
    b = {fg = colors.black,        bg = colors.bright_black                },
    c = {fg = colors.bright_white, bg = colors.xgray1,        gui = 'italic'},
  },
  replace = {
    a = {fg = colors.bright_white, bg = colors.cyan, gui = 'bold' },
    b = {fg = colors.black,        bg = colors.xgray5 },
    c = {fg = colors.bright_white, bg = colors.xgray1, gui = 'italic'},
  },
  visual = {
    a = {fg = colors.black,        bg = colors.cyan, gui = 'bold'},
    b = {fg = colors.bright_white, bg = colors.xgray5},
    c = {fg = colors.bright_white, bg = colors.xgray1, gui = 'italic'},
  },
  inactive = {
    a = {fg = colors.bright_black, bg = colors.xgray2, gui = 'bold'},
    b = {fg = colors.bright_black, bg = colors.xgray2},
    c = {fg = colors.xgray6, bg = colors.xgray2},
  }
}

return srcery
