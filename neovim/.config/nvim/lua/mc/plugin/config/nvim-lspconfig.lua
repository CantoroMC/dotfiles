local lspconfig = require'lspconfig'


-- ON ATTACH FUNCTION: Hooked to all language protocols
local custom_lsp_attach = function(client)
  -- completion
  vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- options
  local filetype = vim.api.nvim_buf_get_option(0, 'filetype')
  local lsp_remap = function(mode, key, lf)
    vim.api.nvim_buf_set_keymap(
      0, -- for the current buffer
      mode,
      key,
      "<Cmd>lua " .. lf .. "<CR>",
      { noremap = true, silent = true }
    )
  end
  local lsp_command = function(name, cmd)
    vim.api.nvim_command(
      string.format(
        'command! -buffer %s :lua vim.%s',
        name, cmd
      )
    )
  end

  if client.config.flags then
    -- Allow using on_line callbacks for lsp
    client.config.flags.allow_incremental_sync = true
  end

  -- Set omni completion (i_CTRL-X_CTRL-O) to the lsp omnifunction
  vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'

  -- Commands
  lsp_command('LspDefinition',      'lsp.buf.definition()')
  lsp_command('LspDeclaration',     'lsp.buf.declaration()')
  lsp_command('LspReferences',      'lsp.buf.references()')
  lsp_command('LspImplementation',  'lsp.buf.implementation()')
  lsp_command('LspTypeDefinition',  'lsp.buf.type_definition()')
  lsp_command('LspRename',          'lsp.buf.rename()')
  lsp_command('LspHover',           'lsp.buf.hover()')
  lsp_command('LspSignatureHelp',   'lsp.buf.signature_help()')
  lsp_command('LspCodeAction',      'lsp.buf.code_action()')

  lsp_command('LspLineDiagnostics', 'lsp.diagnostic.show_line_diagnostics()')
  lsp_command('LspLocationList',    'lsp.diagnostic.set_loclist()')
  lsp_command('LspDocSymbol',       'lsp.buf.document_symbol()')
  lsp_command('LspWsSymbol',        'lsp.buf.workspace_symbol()')

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
  lsp_remap('n', ']g'                         , 'vim.lsp.diagnostic.goto_next()')
  lsp_remap('n', vim.g.maplocalleader..'sd'   , 'vim.lsp.diagnostic.show_line_diagnostics()')
  lsp_remap('n', vim.g.maplocalleader..'dl'   , 'vim.lsp.diagnostic.set_loclist()')
  lsp_remap('n', vim.g.maplocalleader..'dq'   , 'vim.lsp.buf.document_symbol()')
  lsp_remap('n', vim.g.maplocalleader..'ws'   , 'vim.lsp.buf.workspace_symbol()')

  if client.resolved_capabilities.document_formatting then
    -- Formatting mapping
    lsp_remap('n', vim.g.maplocalleader..'gq', 'vim.lsp.buf.formatting()')
    -- Autoformat on save
    -- if vim.tbl_contains({ "cpp" }, filetype) then
    --   vim.api.nvim_exec([[
    --     augroup lsp_format_on_save
    --       autocmd!
    --       autocmd! BufWritePre <buffer> :lua vim.lsp.buf.formatting_sync(nil,1000)
    --     augroup END
    --     ]], false)
    -- end
  end
  if client.resolved_capabilities.document_range_formatting then
    lsp_remap('n', vim.g.maplocalleader..'gw', 'vim.lsp.buf.range_formatting()')
  end
end

-- CAPABILITIES:
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    'documentation',
    'detail',
    'additionalTextEdits',
  }
}

-- Diagnostic Signs
vim.fn.sign_define('LspDiagnosticsSignError',
  { text = '✗', texthl = 'LspDiagnosticsSignError' })
vim.fn.sign_define('LspDiagnosticsSignWarning',
  { text = '', texthl = 'LspDiagnosticsSignWarning' })
vim.fn.sign_define('LspDiagnosticsSignInformation',
  { text = '', texthl = 'LspDiagnosticsSignInformation' })
vim.fn.sign_define('LspDiagnosticsSignHint',
  { text = '', texthl = 'LspDiagnosticsSignHint' })

-- LANGUAGESERVERS:
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
  capabilities = capabilities,
})

--- LUA
lspconfig.sumneko_lua.setup {
  cmd = { 'lua-language-server' };
  filetypes = { 'lua', '-E', '/usr/share/lua-language-server/main.lua' },

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
        enable = true,
        globals = {'vim', 'define', 'it', 'conky'},
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
  capabilities = capabilities,
}

-- SHELL
lspconfig.bashls.setup{
  cmd = { "bash-language-server", "start" },
  cmd_env = {
    GLOB_PATTERN = "*@(.sh|.inc|.bash|.command)"
  },
  filetypes = { "sh", "bash", "zsh" },
  on_attach = custom_lsp_attach,
  capabilities = capabilities,
}

--- TEX
lspconfig.texlab.setup{
  cmd = { 'texlab' },
  filetypes = { 'tex', 'bib', 'plaintex' },
  settings = {
    texlab = {
      auxDirectory = ".",
      build = require'mc.ftplugin.tex.magic_comments'.setup{},
      -- build = {
      --   executable = "latexmk",
      --   args = {
      --     "-pdf",
      --     "-shell-escape",
      --     "-interaction=nonstopmode",
      --     "-synctex=1",
      --     "-file-line-error",
      --     "%f"
      --   },
      --   onSave = false,
      --   forwardSearchAfter = false,
      -- },
      chktex = {
        onOpenAndSave = true,
        onEdit = false,
      },
      diagnosticsDelay = 300,
      forwardSearch = {
        executable = 'zathura',
        args = { "--synctex-forward", "%l:1:%f", "%p" },
        onSave = false,
      },
      bibtexFormatter = "texlab",
      formatterLineLength = 80,
    },
  },
  on_attach = custom_lsp_attach,
  capabilities = capabilities,
}

--- VIM
lspconfig.vimls.setup{
  on_attach = custom_lsp_attach,
  capabilities = capabilities,
}

--[==[ Temporary Taken up by coc

-- GO
lspconfig.gopls.setup{
  cmd = { "gopls" },
  filetypes = { "go", "gomod" },
  root_dir = lspconfig.util.root_pattern("go.mod", ".git", vim.fn.getcwd() ),
  on_attach = custom_lsp_attach,
}

-- PYTHON
lspconfig.pyls.setup{
  cmd = { 'pyls' },
  filetypes = { 'python' },
  on_attach = custom_lsp_attach,
  capabilities = capabilities,
}

-- PERL
lspconfig.perlls.setup{
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

--- HASKELL
lspconfig.hls.setup{
  cmd = { 'haskell-language-server-wrapper', '--lsp' },
  filetypes = { "haskell", "lhaskell" },
  root_dir = lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
  settings = {
    haskell = {
      formattingProvider = 'brittany'
    },
  },
  on_attach = custom_lsp_attach,
  capabilities = capabilities,
}

-- HTML
require'lspconfig'.html.setup {
  capabilities = capabilities,
}

-- CSS
require'lspconfig'.cssls.setup{}

-- Java/TypeScript
require'lspconfig'.tsserver.setup{}

-- Json
require'lspconfig'.jsonls.setup {
  commands = {
    Format = {
      function()
        vim.lsp.buf.range_formatting({},{0,0},{vim.fn.line("$"),0})
      end
    }
  },
  capabilities = capabilities,
}
-- ]==]
