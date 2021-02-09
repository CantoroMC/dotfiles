local lspconfig = require'lspconfig'


-- ON ATTACH FUNCTION: Hooked to all language protocols
local custom_lsp_attach = function(client)
  local filetype = vim.api.nvim_buf_get_option(0, 'filetype')
  local lsp_remap = function(mode, key, lf)
    vim.api.nvim_buf_set_keymap(
      0, -- for the current buffer
      mode,
      key,
      "<Cmd>lua " .. lf .. "<CR>",
      {noremap = true, silent = true}
    )
  end

  if client.config.flags then
    -- Allow using on_line callbacks for lsp
    client.config.flags.allow_incremental_sync = true
  end

  -- Set omni completion (i_CTRL-X_CTRL-O) to the lsp omnifunction
  vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'

  -- KeyBindings
  lsp_remap('n', vim.g.maplocalleader..'<C-]>', 'vim.lsp.buf.definition()')
  lsp_remap('n', vim.g.maplocalleader..'<C-[>', 'vim.lsp.buf.declaration()')
  lsp_remap('n', vim.g.maplocalleader..'gr'   , 'vim.lsp.buf.references()')
  lsp_remap('n', vim.g.maplocalleader..'gd'   , 'vim.lsp.buf.implementation()')
  lsp_remap('n', vim.g.maplocalleader..'gD'   , 'vim.lsp.buf.type_definition()')
  lsp_remap('n', vim.g.maplocalleader..'rw'   , 'vim.lsp.buf.rename()')
  lsp_remap('n', vim.g.maplocalleader..'H'    , 'vim.lsp.buf.hover()')
  lsp_remap('n', vim.g.maplocalleader..'K'    , 'vim.lsp.buf.signature_help()')
  lsp_remap('n', vim.g.maplocalleader..'A'    , 'vim.lsp.buf.code_action()')

  lsp_remap('n', '[g'                         , 'vim.lsp.diagnostic.goto_prev()')
  lsp_remap('n', ']d'                         , 'vim.lsp.diagnostic.goto_next()')
  lsp_remap('n', vim.g.maplocalleader..'sd'   , 'vim.lsp.diagnostic.show_line_diagnostics()')
  lsp_remap('n', vim.g.maplocalleader..'dl'   , 'vim.lsp.diagnostic.set_loclist()')
  lsp_remap('n', vim.g.maplocalleader..'dq'   , 'vim.lsp.diagnostic.document_symbol()')

  lsp_remap('n', vim.g.maplocalleader..'af'   , 'vim.lsp.buf.add_workspace_folder()')
  lsp_remap('n', vim.g.maplocalleader..'rf'   , 'vim.lsp.buf.remove_workspace_folder()')
  lsp_remap('n', vim.g.maplocalleader..'pf'   , 'print(vim.inspect(vim.lsp.buf.list_workspace_folders()))')

  -- TODO: Define LspCommands

  if client.resolved_capabilities.document_formatting then
    -- Formatting mapping
    lsp_remap('n', vim.g.maplocalleader..'gq', 'vim.lsp.buf.formatting()')
    -- Autoformat on save
    if vim.tbl_contains({"go", "haskell"}, filetype) then
      vim.api.nvim_exec([[
        augroup lsp_format_on_save
          autocmd!
          autocmd! BufWritePre <buffer> :lua vim.lsp.buf.formatting_sync()
        augroup END
        ]], false)
    end
  end
  if client.resolved_capabilities.document_range_formatting then
    lsp_remap('n', vim.g.maplocalleader..'gw', 'vim.lsp.buf.range_formatting()')
  end
end


-- C,CPP
lspconfig.clangd.setup({
  cmd = {
    "clangd",
    "--background-index",
    "--suggest-missing-includes",
    "--clang-tidy",
    "--header-insertion=iwyu",
  },
  filetypes = { "c", "cpp", "objc", "objcpp" },
  on_attach = custom_lsp_attach,
})

-- GO
lspconfig.gopls.setup{
  cmd = { "gopls" },
  filetypes = { "go", "gomod" },
  root_dir = lspconfig.util.root_pattern("go.mod", ".git", vim.fn.getcwd() ),
  on_attach = custom_lsp_attach,
}

-- LUA
lspconfig.sumneko_lua.setup {
  cmd = { 'lua-language-server' };
  filetypes = { 'lua' },

  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },

      completion = {
        keywordSnippet = { "Both" },
      },

      hint = {
        enable = true,
        setType = true,
      },

      diagnostics = {
        globals = {'vim', 'define', 'it'},
        disable = { "lowercase-global" },
      },

      workspace = {
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
    },
  },
  on_attach = custom_lsp_attach,
}

-- HASKELL
lspconfig.hls.setup{
  cmd = { 'haskell-language-server-wrapper', '--lsp' },
  filetypes = { "haskell", "lhaskell" },
  root_dir = lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", vim.fn.getcwd() ),
  settings = {
    haskell = {
      formattingProvider = 'brittany'
    },
  },
  on_attach = custom_lsp_attach,
}

-- PERL
lspconfig.perlls.setup{
  on_attach = custom_lsp_attach,
}

-- PYTHON
lspconfig.pyls.setup{
  cmd = { 'pyls' },
  filetypes = { 'python' },
  on_attach = custom_lsp_attach,
}

-- RUBY
lspconfig.solargraph.setup{
  cmd = { "solargraph", "stdio" },
  filetypes = { "ruby" },
  root_dir = lspconfig.util.root_pattern("Gemfile", ".git", vim.fn.getcwd() ),
  settings = {
    solargraph = {
      checkGemVersion = true,
      completion = true,
      definitions = true,
      diagnostics = true,
      folding = true,
      formatting = true,
      hover = true,
      references = true,
      rename = true,
      symbols = true,
    }
  },
  on_attach = custom_lsp_attach,
}

-- SHELL
lspconfig.bashls.setup{
  cmd = { "bash-language-server", "start" },
  cmd_env = {
    GLOB_PATTERN = "*@(.sh|.inc|.bash|.command)"
  },
  filetypes = { "sh", "bash", "zsh" },
  on_attach = custom_lsp_attach,
}

-- TEX
lspconfig.texlab.setup{
  cmd = { 'texlab' },
  filetypes = { 'tex', 'bib', 'plaintex' },
  settings = {
    bibtex = {
      formatting = {
        lineLenght = 120,
        formatter = 'texlab',
      },
    },
    latex = {
      build = {
        args = {
          "-pdf",
          "-shell-escape",
          "-interaction=nonstopmode",
          "-synctex=1",
          "-file-line-error",
          "%f"
        },
        executable = "latexmk",
        onSave = false,
      },
      forwardSearch = {
        args = { '%p' },
        onSave = false,
        executable = 'zathura',
      },
      lint = {
        onChange = true,
        onSave = true,
      }
    },
  },
  on_attach = custom_lsp_attach,
}

-- VIM
lspconfig.vimls.setup{
  on_attach = custom_lsp_attach,
}
