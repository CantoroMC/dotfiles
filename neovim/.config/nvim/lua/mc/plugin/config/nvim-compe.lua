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
    path            = true;
    buffer          = true;
    spell           = true;
    tags            = true;
    calc            = false;
    emoji           = true;

    nvim_lsp        = true;
    nvim_lua        = true;

    snippets_nvim   = false;
    vsnip           = false;
    ultisnips       = true;
    nvim_treesitter = false;

    zsh             = true;
    latex_symbols   = {
     priority = 999
    };
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
