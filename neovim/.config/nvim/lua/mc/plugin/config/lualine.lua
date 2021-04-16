local ayu = require'lualine.themes.ayu_dark'

-- -- Chnage the background of lualine_c section for normal mode
ayu.normal.a.bg = '#b8cc52' -- rgb colors are supported
ayu.insert.a.bg = '#36a3d9' -- rgb colors are supported


require('lualine').setup{
  options = {
    theme                = ayu,
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
        file_status = true,
        full_path   = true,
        shorten     = true,
        right_padding = 0,
      },
      {
        require'mc.plugin.statusline'.file_size,
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
      require'mc.plugin.statusline'.vista,
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
      require'mc.plugin.statusline'.whitespaces,
    },
    lualine_z = {
      {
        'location',
        icon = '',
        right_padding = 0,
      },
      {
        require'mc.plugin.statusline'.nr_lines,
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
  extensions = { 'fzf', 'nvim-tree' }
}
