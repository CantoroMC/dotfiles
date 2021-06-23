require'nvim-treesitter.configs'.setup {
  ensure_installed = 'maintained',
  -- TreeSitter: {{{1
  -- HIGHLIGHT: {{{2
  highlight = {
    enable = true,
    disable = {"bash"},
    use_languagetree = false,
    -- custom_captures = {
      -- ["capture.group"] = "HighlightGroup",
    -- },
  },
  -- }}}
  -- INC_SEL: {{{2
  incremental_selection = {
    enable = true,
    disable = {},
    keymaps = { -- mappings for incremental selection (visual mappings)
      init_selection    = '<M-n>',   -- maps in normal mode to init the node/scope selection
      node_incremental  = '<M-n>',   -- increment to the upper named parent
      scope_incremental = '<M-S-n>', -- increment to the upper scope (as defined in locals.scm)
      node_decremental  = '<M-p>',   -- decrement to the previous node
    },
  },
  -- }}}
  -- INDENT: {{{2
  indent = {
    enable = true,
    disable = {"python"},
  },
  -- }}}
  -- }}}
  -- Refactor: {{{1
  refactor = { -- Tree-Sitter Refactor
    highlight_definitions = { enable = true },
    highlight_current_scope = { enable = false },

    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = 'grr',
      },
    },

    navigation = {
      enable = true,
      keymaps = {
        goto_definition = 'gnd',
        list_definitions = '<LocalLeader>ld',
        list_definitions_toc = "<LocalLeader>lt",
        goto_next_usage = "<M-]>",
        goto_previous_usage = "<M-[>",
      },
    },
  },
  -- }}}
  -- TextObjects {{{1
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['aC'] = '@class.outer',
        ['iC'] = '@class.inner',
        ['ac'] = '@conditional.outer',
        ['ic'] = '@conditional.inner',
        ['ab'] = '@block.outer',
        ['ib'] = '@block.inner',
        ['al'] = '@loop.outer',
        ['il'] = '@loop.inner',
        ['is'] = '@statement.inner',
        ['as'] = '@statement.outer',
        ['ah'] = '@comment.outer',
        ['ad'] = '@call.outer',
        ['id'] = '@call.inner',
        -- ['iL'] = { -- you can define your own textobjects directly here
        --   python = '(function_definition) @function',
        --   cpp = '(function_definition) @function',
        --   c = '(function_definition) @function',
        --   java = '(method_declaration) @function',
        -- },
      },
    },

    swap = {
      enable = true,
      swap_next = {
        ["<A-.>"] = "@parameter.inner",
      },
      swap_previous = {
        ["<A-,>"] = "@parameter.inner",
      },
    },

    move = {
      enable = true,
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },

    lsp_interop = {
      enable = true,
      peek_definition_code = {
        ["<Leader>pf"] = "@function.outer",
        ["<Leader>pF"] = "@class.outer",
      },
    },
  },
  -- }}}
  -- Playground {{{1
  playground = { -- Tree-Sitter Playground
    enable = true,
    disable = {},
    updatetime = 25,
    persist_queries = false,
  },
  query_linter = { -- Tree-Sitter Playground
    enable = true,
    use_virtual_text = true,
    lint_events = {"BufWrite", "CursorHold"},
  },
  -- }}}
  -- Rainbow Parenthesis {{{1
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = 1500,
  },
  -- }}}
}

-- vim:fdm=marker
