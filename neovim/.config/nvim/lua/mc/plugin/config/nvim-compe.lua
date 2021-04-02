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
    -- Builtin Sources
    path            = true;
    buffer          = true;
    spell           = true;
    tags            = true;
    calc            = false;
    emoji           = true;

    -- Builting Lsp
    nvim_lsp        = true;
    nvim_lua        = true;

    -- Builting Plugins
    snippets_nvim   = false;
    vsnip           = false;
    ultisnips       = true;
    nvim_treesitter = false;

    -- External Sources
    zsh             = true;
    latex_symbols   = {
     priority = 999;
     filetypes = { 'tex', 'plaintex', 'bibtex' };
    };

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
