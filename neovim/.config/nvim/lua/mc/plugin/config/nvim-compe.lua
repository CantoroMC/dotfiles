require'compe'.register_source('email', require'mc.plugin.config.nvim-compe.email')

require'compe'.setup {
  enabled          = true;
  autocomplete     = true;
  debug            = false;
  min_length       = 1;
  preselect        = 'enable';
  throttle_time    = 80;
  source_timeout   = 200;
  incomplete_delay = 400;
  max_abbr_width   = 100;
  max_kind_width   = 100;
  max_menu_width   = 100;
  documentation    = true;

  source = {
    -- Builtin Sources (kind and menu entries)
    path = {
      menu = " "
    };
    buffer = {
      dup = false;
      menu = "B"
    };
    spell = {
      dup = false;
      menu = "﬜",
    };
    tags = {
      dup = false;
      menu = "T",
    };
    emoji = {
      menu = "ﲃ",
    };
    calc = false;

    -- Builting Lsp
    nvim_lsp = {
      priority = 2;
      menu = "撚",
    };
    nvim_lua = {
      priority = 5;
      menu = "",
    };

    -- Builting Plugins
    ultisnips = {
      priority = 1;
      dup = true;
      menu = "";
    };
    nvim_treesitter = {
      priority = 10;
      dup = false;
      menu = "";
    };
    snippets_nvim = false;
    vsnip = false;

    -- External Sources
    zsh = {
      priority = 10;
      filetypes = { 'zsh' };
      dup = false;
      menu = '';
    };
    latex_symbols = false;
    --[[
    latex_symbols = {
      enable = false;
      priority = 999;
      filetypes = { 'tex', 'plaintex', 'bibtex' };
      dup = false;
      menu = "﨧";
    };
    --]]

    -- My sources
    email = true;
  };
}

require'lspkind'.init{
  with_text = true,
  symbol_map = {
    Text        = '',
    Method      = 'ƒ',
    Function    = '',
    Constructor = '',
    Variable    = '',
    Class       = '',
    Interface   = 'ﰮ',
    Module      = '',
    Property    = '',
    Unit        = '塞',
    Value       = '',
    Enum        = '練',
    Keyword     = '',
    Snippet     = 'זּ',
    Color       = '',
    File        = '',
    Folder      = 'ﱮ',
    EnumMember  = '',
    Constant    = '',
    Struct      = ''
  },
}
