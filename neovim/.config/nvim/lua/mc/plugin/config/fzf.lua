local wk = require'which-key'

wk.register(
  {
    name = "FuzzyFinder",
     a = { "<cmd>Ag<CR>",         "Ag wrapper" },
     b = { "<cmd>Buffers<CR>",    "Buffers" },
     B = { "<cmd>Bins<CR>",       "User binaries" },
     c = { "<cmd>BibtexCite<CR>", "bibtex-ls wrapper" },
     d = { "<cmd>Dotfiles<CR>",   "Dotfiles" },
     f = { "<cmd>Files<CR>",      "Files" },
     g = { "<cmd>GFiles<CR>",     "Git files" },
     h = { "<cmd>History<CR>",    "Previous opened files" },
     l = { "<cmd>BLines<CR>",     "Current buffer lines" },
     L = { "<cmd>Lines<CR>",      "All buffers lines" },
     p = { "<cmd>VimConfigs<CR>", "Neovim config files" },
     r = { "<cmd>Rg<CR>",         "Ripgrep wrapper" },
     s = { "<cmd>Skeletons<CR>",  "Skeletons" },
     t = { "<cmd>Tags<CR>",       "Ctags weapper" },
     v = { "<cmd>VimData<CR>",    "Neovim data files" },
  },
  {
    mode    = "n",
    prefix  = "<C-c><C-z>",
    silent  = true,
    noremap = true,
    nowait  = false,
  }
)
