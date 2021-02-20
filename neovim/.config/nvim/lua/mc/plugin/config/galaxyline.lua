local gl = require('galaxyline')

local buffer_not_empty    = require'galaxyline.condition'.buffer_not_empty
local check_git_workspace = require'galaxyline.condition'.check_git_workspace
local hide_in_width       = require'galaxyline.condition'.hide_in_width


gl.short_line_list = { 'NvimTree','vista','fugitive' }
local blocks = gl.section
local colors = {
  bg         = '#151a1e',
  fg         = '#eaeaea',
  yellow     = '#e7c547',
  cyan       = '#95e6cb',
  darkblue   = '#0F1419',
  green      = '#b8cc52',
  orange     = '#FF8800',
  violet     = '#a9a1e1',
  magenta    = '#c678dd',
  blue       = '#36a3d9';
  red        = '#ff3333';
}

blocks.left[1] = {
  Intro = {
    provider = function()
      return '▊ '
    end,
    highlight = {colors.green,colors.bg}
  },
}

blocks.left[2] = {
  ViMode = {
    provider = function()
      local modes = {
        ['?']  = { text = '???',          highlight = colors.red },
        ['n']  = { text = 'NORMAL',       highlight = colors.fg },
        ['i']  = { text = 'INSERT',       highlight = colors.yellow },
        ['v']  = { text = 'VISUAL',       highlight = colors.blue },
        ['V']  = { text = 'V-LINE',       highlight = colors.blue },
        [''] = { text = 'V-BLOCK',      highlight = colors.blue },
        ['c']  = { text = 'COMMAND',      highlight = colors.green },
        ['no'] = { text = 'OPERATOR',     highlight = colors.green },
        ['s']  = { text = 'SELECT',       highlight = colors.orange },
        ['S']  = { text = 'S-LINE',       highlight = colors.orange },
        [''] = { text = 'S-BLOCK',      highlight = colors.orange },
        ['ic'] = { text = 'INS-COMP',     highlight = colors.yellow },
        ['R']  = { text = 'REPLACE',      highlight = colors.violet },
        ['Rv'] = { text = 'VIRT-REPLACE', highlight = colors.violet },
        ['cv'] = { text = 'VIM-EX',       highlight = colors.red },
        ['ce'] = { text = 'NORMAL-EX',    highlight = colors.red },
        ['r']  = { text = 'HIT-ENTER',    highlight = colors.cyan },
        ['rm'] = { text = 'MORE',         highlight = colors.cyan },
        ['r?'] = { text = 'CONFIRM',      highlight = colors.cyan },
        ['!']  = { text = 'SHELL',        highlight = colors.red },
        ['t']  = { text = 'TERMINAL',     highlight = colors.green },
      }
      local mode = modes[vim.fn.mode()] or modes['?']
      vim.api.nvim_command('hi GalaxyViMode  guifg='..mode.highlight)
      return '  ' .. mode.text .. ' '
    end,
    highlight = {
      colors.red,
      colors.bg,
      'bold'
    },
  },
}

--- FileInfo
blocks.left[3] ={
  FileIcon = {
    provider = 'FileIcon',
    condition = buffer_not_empty,
    highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,colors.bg},
  },
}

blocks.left[4] = {
  FileName = {
    provider = {'FileName'},
    condition = buffer_not_empty,
    separator = '|',
    highlight = {colors.green,colors.bg,'bold'}
  }
}

blocks.left[5] = {
  FileType = {
    provider = function()
      return vim.bo.filetype
    end,
    separator = '|',
    condition = buffer_not_empty,
    highlight = {colors.green,colors.bg,'bold'}
  }
}

blocks.left[6] = {
  FileSize = {
    provider = 'FileSize',
    condition = buffer_not_empty,
    highlight = {colors.fg,colors.bg}
  }
}


blocks.left[7] = {
  LineInfo = {
    provider = 'LineColumn',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg},
  },
}

blocks.left[8] = {
  TotLines = {
    provider = function ()
      return '(' .. vim.fn.line('$') .. ')'
    end,
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg,'italic'},
  }
}

blocks.left[9] = {
  PerCent = {
    provider = 'LinePercent',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg,'bold'},
  }
}

blocks.left[10] = {
  ScrollBar = {
    provider = 'ScrollBar',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg,'bold'}
  }
}

blocks.left[11] = {
  FileEncode = {
    provider = function()
      local is_utf8 = (vim.bo.fileencoding == '' or vim.bo.fileencoding == vim.o.encoding)
      local encode = is_utf8 and '' or vim.bo.fenc
      return ' ' .. encode:upper()
    end,
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.cyan,colors.bg,'bold'}
  }
}

blocks.left[12] = {
  FileFormat = {
    provider = function()
      local is_utf8 = (vim.bo.fileencoding == '' or vim.bo.fileencoding == vim.o.encoding)
      local encode = is_utf8 and '' or vim.bo.fenc
    end,
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.cyan,colors.bg,'bold'}
  }
}

--- Vista
blocks.left[12] = {
  VistaPlugin = {
    provider = 'VistaPlugin',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.blue,colors.bg},
  }
}

--- Right Side
blocks.right[1] = {
  WhiteSpace = {
    provider = 'WhiteSpace',
    highlight = {colors.red,colors.bg}
  }
}

--- Diagnostic

blocks.right[2] = {
  DiagnosticError = {
    provider = 'DiagnosticError',
    icon = '  ',
    highlight = {colors.red,colors.bg}
  }
}

blocks.right[3] = {
  DiagnosticWarn = {
    provider = 'DiagnosticWarn',
    icon = '  ',
    highlight = {colors.yellow,colors.bg},
  }
}

blocks.right[4] = {
  DiagnosticHint = {
    provider = 'DiagnosticHint',
    icon = '  ',
    highlight = {colors.cyan,colors.bg},
  }
}

blocks.right[5] = {
  DiagnosticInfo = {
    provider = 'DiagnosticInfo',
    icon = '  ',
    highlight = {colors.blue,colors.bg},
  }
}

--- Git
blocks.right[6] = {
  GitIcon = {
    provider = function() return ' ' end,
    condition = check_git_workspace,
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.red,colors.bg,'bold'},
  }
}

blocks.right[7] = {
  GitBranch = {
    provider = 'GitBranch',
    condition = check_git_workspace,
    highlight = {colors.red,colors.bg,'bold'},
  }
}

blocks.right[8] = {
  DiffAdd = {
    provider = 'DiffAdd',
    condition = hide_in_width,
    icon = '  ',
    highlight = {colors.green,colors.bg},
  }
}
blocks.right[9] = {
  DiffModified = {
    provider = 'DiffModified',
    condition = hide_in_width,
    icon = ' 柳',
    highlight = {colors.orange,colors.bg},
  }
}
blocks.right[10] = {
  DiffRemove = {
    provider = 'DiffRemove',
    condition = hide_in_width,
    icon = '  ',
    highlight = {colors.red,colors.bg},
  }
}
blocks.right[11] = {
  Outro = {
    provider = function() return ' ▊' end,
    highlight = {colors.green,colors.bg}
  },
}


--- Short Version
blocks.short_line_left[1] = {
  BufferType = {
    provider = 'FileTypeName',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.blue,colors.bg,'bold'}
  }
}

blocks.short_line_left[2] = {
  SFileName = {
    provider = function ()
      local fileinfo = require('galaxyline.provider_fileinfo')
      local fname = fileinfo.get_current_file_name()
      for _,v in ipairs(gl.short_line_list) do
        if v == vim.bo.filetype then
          return ''
        end
      end
      return fname
    end,
    condition = buffer_not_empty,
    highlight = {colors.white,colors.bg,'bold'}
  }
}

blocks.short_line_right[1] = {
  BufferIcon = {
    provider= 'BufferIcon',
    highlight = {colors.fg,colors.bg}
  }
}
