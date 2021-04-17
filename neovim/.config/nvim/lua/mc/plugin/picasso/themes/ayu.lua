local ayu = {}

local colors = {
  base0   = '#151a1e',
  base1   = '#1c2328',
  base2   = '#232b32',
  base3   = '#2a343c',
  base6   = '#3f4e5a',
  orange  = '#ff7733',
  yellow  = '#e7c547',
  blue    = '#36a3d9',
  cyan    = '#95e6cb',
  green   = '#b8cc52',
}

ayu.normal = {
  a = {fg = colors.base3, bg = colors.green, gui = 'bold'  },
  b = {fg = colors.green, bg = colors.base3                },
  c = {fg = colors.cyan,  bg = colors.base0, gui = 'italic'},
}

ayu.insert = {
  a = {fg = colors.base3, bg = colors.blue,  gui = 'bold'  },
  b = {fg = colors.blue,  bg = colors.base3                },
  c = {fg = colors.cyan,  bg = colors.base0, gui = 'italic'},
}

ayu.replace = {
  a = {fg = colors.base2,  bg = colors.orange, gui = 'bold' },
  b = {fg = colors.orange, bg = colors.base3                },
  c = {fg = colors.cyan,   bg = colors.base0, gui = 'italic'},
}

ayu.visual = {
  a = {fg = colors.base6,  bg = colors.yellow, gui = 'bold'},
  b = {fg = colors.yellow, bg = colors.base6},
  c = {fg = colors.cyan,   bg = colors.base0, gui = 'italic'},
}

ayu.inactive = {
  a = {fg = colors.blue, bg = colors.base2, gui = 'bold'},
  b = {fg = colors.blue, bg = colors.base1},
  c = {fg = colors.blue, bg = colors.base0},
}

return ayu
