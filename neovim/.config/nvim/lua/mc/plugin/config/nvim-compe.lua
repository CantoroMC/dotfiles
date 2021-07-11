local folderOfThisFile = (...):match("(.-)[^%.]+$")

require'compe'.register_source('email',
  require(folderOfThisFile .. 'nvim-compe.email'))

require'compe'.setup {
  enabled          = true,
  autocomplete     = true,
  debug            = false,
  min_length       = 1,
  preselect        = 'enable',
  throttle_time    = 80,
  source_timeout   = 200,
  incomplete_delay = 400,
  max_abbr_width   = 100,
  max_kind_width   = 100,
  max_menu_width   = 100,
  documentation = {
    border = "none",
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  },
  source = {
    -- Builtin Sources (kind and menu entries)
    path = {
      kind = "ﱮ",
      dup = false,
      priority = 10,
    },
    buffer = {
      kind = "Buffer",
      menu = "",
      dup = false,
      priority = 50,
    },
    spell = {
      kind = "Spell",
      menu = "﬜",
      dup = false,
      priority = 10,
    },
    tags = {
      kind = "Tags",
      menu = "",
      dup = false,
      priority = 10,
    },
    emoji = {
      kind = "Emoji",
      menu = "ﲃ",
      priority = 10,
    },
    calc = false,

    -- Builting Lsp
    nvim_lsp = {
      menu = "LSP",
      dup = true,
      priority = 100,
    },
    nvim_lua = {
      priority = 90,
      menu = "",
      dup = false,
    },

    -- Builtin Plugins
    ultisnips = {
      menu = "זּ",
      dup = true,
      priority = 70,
    },
    nvim_treesitter = {
      menu = "",
      kind = "TreeSitter",
      dup = true,
      priority = 90,
    },
    snippets_nvim = false,
    vsnip = false,

    -- External Sources
    zsh = {
      priority = 70,
      filetypes = { 'zsh', 'sh', 'bash' },
      dup = false,
      menu = '',
    },
    tmux = {
      priority = 20,
      dup = false,
      menu = '﮸',
      disabled = false,
      all_panes = false,
    },
    latex_symbols = false,
    -- latex_symbols = {
    --   enable = true,
    --   menu = "",
    --   kind = "TeX Symb",
    --   priority = 100000,
    --   filetypes = { 'tex', 'plaintex', 'bibtex' },
    --   dup = false,
    -- },

    -- My sources
    email = true,
  },
}
