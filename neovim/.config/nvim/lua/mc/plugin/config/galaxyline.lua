local gl = require('galaxyline')
local blocks = gl.section
gl.short_line_list = {'NvimTree','vista','dbui'}

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
      -- auto change color according the vim mode
      local mode_color = {
        n      = colors.magenta,
        i      = colors.green,
        v      = colors.blue,
        [''] = colors.blue,
        V      = colors.blue,
        c      = colors.red,
        no     = colors.magenta,
        s      = colors.orange,
        S      = colors.orange,
        [''] = colors.orange,
        ic     = colors.yellow,
        R      = colors.violet,
        Rv     = colors.violet,
        cv     = colors.red,
        ce     = colors.red,
        r      = colors.cyan,
        rm     = colors.cyan,
        ['r?'] = colors.cyan,
        ['!']  = colors.red,
        t      = colors.red
      }
      vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
      return '  '
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
    condition = require'galaxyline.condition'.buffer_not_empty,
    highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,colors.bg},
  },
}

blocks.left[4] = {
  FileName = {
    provider = {'FileName'},
    condition = require'galaxyline.condition'.buffer_not_empty,
    highlight = {colors.green,colors.bg,'bold'}
  }
}

blocks.left[5] = {
  FileSize = {
    provider = 'FileSize',
    condition = require'galaxyline.condition'.buffer_not_empty,
    highlight = {colors.fg,colors.bg}
  }
}

blocks.left[6] = {
  LineInfo = {
    provider = 'LineColumn',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg},
  },
}

blocks.left[7] = {
  PerCent = {
    provider = 'LinePercent',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg,'bold'},
  }
}

blocks.left[8] = {
  ScrollBar = {
    provider = 'ScrollBar',
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.fg,colors.bg,'bold'}
  }
}

blocks.left[9] = {
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

blocks.right[10] = {
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
    provider = function() return '  ' end,
    condition = require('galaxyline.provider_vcs').check_git_workspace,
    separator = ' ',
    separator_highlight = {'NONE',colors.bg},
    highlight = {colors.violet,colors.bg,'bold'},
  }
}

blocks.right[7] = {
  GitBranch = {
    provider = 'GitBranch',
    condition = require('galaxyline.provider_vcs').check_git_workspace,
    highlight = {colors.violet,colors.bg,'bold'},
  }
}

blocks.right[8] = {
  DiffAdd = {
    provider = 'DiffAdd',
    condition = require'galaxyline.condition'.hide_in_width,
    icon = '  ',
    highlight = {colors.green,colors.bg},
  }
}
blocks.right[9] = {
  DiffModified = {
    provider = 'DiffModified',
    condition = require'galaxyline.condition'.hide_in_width,
    icon = ' 柳',
    highlight = {colors.orange,colors.bg},
  }
}
blocks.right[10] = {
  DiffRemove = {
    provider = 'DiffRemove',
    condition = require'galaxyline.condition'.hide_in_width,
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
    condition = require'galaxyline.condition'.buffer_not_empty,
    highlight = {colors.white,colors.bg,'bold'}
  }
}

blocks.short_line_right[1] = {
  BufferIcon = {
    provider= 'BufferIcon',
    highlight = {colors.fg,colors.bg}
  }
}
