require'which-key'.setup {
  plugins = {
    marks     = true,
    registers = true,
    spelling  = {
      enabled     = true,
      suggestions = 30,
    },
    presets   = {
      operators    = true,
      motions      = true,
      text_objects = true,
      windows      = true,
      nav          = true,
      z            = true,
      g            = true,
    },
  },
  operators = { gc = "Comments" },
  icons = {
    breadcrumb = "»",
    separator = "➜",
    group = " ",
  },
  window = {
    border   = "none",   -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin   = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]
    padding  = { 1, 1, 1, 1 }, -- extra window padding [top, right, bottom, left]
  },
  layout = {
    height  = { min =  4, max = 25 }, -- min and max height of the columns
    width   = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = {
    "<silent>",
    "<cmd>",
    "<Cmd>",
    "<CR>",
    "call",
    "lua",
    "^:",
    "^ ",
    "<Plug>",
  },
  show_help = true,
  triggers = "auto",
  triggers_blacklist = {
    i = { "j", "k" },
    v = { "j", "k" },
  },
}
