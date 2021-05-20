require'mc.plugin.picasso'.setup()

local folderOfThisFile = (...):match("(.-)[^%.]+$")
local monitors         = require(folderOfThisFile .. 'lualine.monitors')
local theme            = require(string.format(
  '%slualine.themes.%s',
  folderOfThisFile,
  require'mc.plugin.picasso'.lualine()
))

require'lualine'.setup{
  options = {
    theme                = theme,
    section_separators   = {'', ''},
    component_separators = {'', ''},
    icons_enabled        = true,
    padding              = 1,
  },
  sections = {
    lualine_a = { -- Vim Mode
      {
        'mode',
        format = function(mode_name) return mode_name:sub(1,1) end,
      }
    },
    lualine_b = { -- VCS infos
      {
        'branch',
        icon = '',
      },
      {
        'diff',
        colored = true,
        symbols = { added = ' ', modified = '柳', removed = ' '}
      }
    },
    lualine_c = { -- File name and size
      {
        'filename',
        file_status   = true,
        path          = 1,
        right_padding = 0,
      },
      {
        monitors.file_size,
        left_padding = 0,
      }
    },
    lualine_x = {
      {
        'diagnostics',
        sources  = { 'nvim_lsp', 'coc' },
        sections = { 'error', 'warn', 'info' },
        symbols  = { error = ' ', warn = ' ', info = ' ' }
      },
      monitors.vista,
    },
    lualine_y = {
      {
        'encoding',
        format = function(enc)
          if enc == 'utf-8' then
            return ''
          end
        end,
      },
      {
        'fileformat',
        format = function(ff) return ff:gsub('unix','') end,
        icons_enabled = false,

      },
      'filetype',
      monitors.whitespaces,
    },
    lualine_z = {
      {
        'location',
        icon = '',
        right_padding = 0,
      },
      {
        monitors.nr_lines,
        left_padding = 0,
      },
    },
  },
  inactive_sections = {
    lualine_a = {  },
    lualine_b = {  },
    lualine_c = { 'filename' },
    lualine_x = { 'location' },
    lualine_y = {  },
    lualine_z = {   }
  },
  extensions = { 'fzf', 'nvim-tree', 'fugitive', 'quickfix' }
}
