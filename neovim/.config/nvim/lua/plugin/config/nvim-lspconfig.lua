require'lspconfig'.sumneko_lua.setup { -- LUA
  cmd = { 'lua-language-server' };

  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },

      diagnostics = {
        globals = {'vim', 'define', 'it'},
      },

      workspace = {
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
    },
  },

  on_attach=require'completion'.on_attach,
}
