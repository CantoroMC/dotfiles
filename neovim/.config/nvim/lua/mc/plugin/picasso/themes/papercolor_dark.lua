local papercolor = {}

local colors = {
  bg         = '#1c1c1c',
  normal     = '#5a6244',
  fg         = '#21221d',
  insert     = '#7CAFC2',
  visual     = '#BA8BAF',
  replace    = '#AB4642',
  alt_fg     = '#282828',
  git_fg     = '#B8B8B8',
  git_bg     = '#383838',
}

papercolor.normal = {
  a = {fg = colors.fg,     bg = colors.normal, gui = 'bold'  },
  b = {fg = colors.git_fg, bg = colors.git_bg                },
  c = {fg = colors.normal, bg = colors.bg,     gui = 'italic'},
}

papercolor.insert = {
  a = {fg = colors.alt_fg, bg = colors.insert, gui = 'bold'  },
  b = {fg = colors.git_fg, bg = colors.git_bg                },
  c = {fg = colors.insert, bg = colors.bg,     gui = 'italic'},
}

papercolor.replace = {
  a = {fg = colors.alt_fg,  bg = colors.replace, gui = 'bold'   },
  b = {fg = colors.git_fg,  bg = colors.git_bg                  },
  c = {fg = colors.replace, bg = colors.bg,      gui = 'italic' },
}

papercolor.visual = {
  a = {fg = colors.alt_fg, bg = colors.visual, gui = 'bold'  },
  b = {fg = colors.git_fg, bg = colors.git_bg                },
  c = {fg = colors.visual, bg = colors.bg,     gui = 'italic'},
}

papercolor.inactive = {
  a = {fg = colors.fg, bg = colors.git_fg, gui = 'bold'},
  b = {fg = colors.fg, bg = colors.alt_fg              },
  c = {fg = colors.fg, bg = colors.bg                  },
}

return papercolor
