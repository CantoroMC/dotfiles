local telescope = require'telescope'
telescope.setup{
  defaults = {
    -- Layout
    initial_mode    = "insert",
    scroll_startegy = 'cycle',
    prompt_position = "top",
    prompt_prefix   = "❯ ",
    selection_caret = "❯ ",
    entry_prefix    = "  ",
    winblend        = 10,
    border          = true,
    borderchars = {
      "z",
      prompt  = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { '═', ' ', ' ', ' ', ' ', ' ', ' ', ' ' },
    },
    color_devicons  = true,
    -- IN PROGRESS:
    -- width = 0.90,
    preview_cutoff = 1,
    -- results_height = 1,
    results_width = 0.4,
    layout_strategy = "bottom_pane",
    layout_config = {
      height = 25,
    },
    layout_defaults = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = false,
      },
    },
    -- Sorting
    selection_strategy   = "reset",
    sorting_strategy     = "ascending",
    file_sorter          = require'telescope.sorters'.get_fuzzy_file,
    generic_sorter       = require'telescope.sorters'.get_generic_fuzzy_sorter,
    file_ignore_patterns = {},
    shorten_path         = true,
    -- Searching
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    -- Preview
    use_less         = true,
    set_env          = { ['COLORTERM'] = 'truecolor' },
    file_previewer   = require'telescope.previewers'.vim_buffer_cat.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker
  }
}

local folderOfThisFile = (...):match("(.-)[^%.]+$")
luv_picker = require(folderOfThisFile..'telescope.picker')

-- Mappings via WhichKey
vim.api.nvim_set_keymap('n', '<C-c><C-x>', '<Cmd>Telescope<CR>',
  { noremap = true, silent = true })
local wk = require'which-key'
wk.register(
  {
    name = "Telescope",
    a = { "<cmd>Telescope current_buffer_fuzzy_find<CR>", "Fuzzy find current buffer" },
    b = { "<cmd>Telescope buffers<CR>",                   "List buffers" },
    c = { "<cmd>Telescope command<CR>",                   "Vim commands" },
    d = { "<cmd>lua luv_picker.dotfiles()<CR>",                   "Dotfiles" },
    f = { "<cmd>Telescope find_files<CR>",                "Files in pwd" },
    F = { "<cmd>Telescope file_browser<CR>",              "Fuzzy browser" },
    g = { "<cmd>Telescope live_grep<CR>",                 "Live grep in pwd" },
    G = { "<cmd>Telescope git_files<CR>",                 "Git files" },
    k = { "<cmd>Telescope keymaps<CR>",                   "Vim maps" },
    m = { "<cmd>Telescope man_pages<CR>",                 "Man pages" },
    M = { "<cmd>Telescope marks<CR>",                     "Vim marks" },
    n = { "<cmd>Telescope grep_string<CR>",               "Grep under cursor in pwd" },
    r = { "<cmd>Telescope registers<CR>",                 "Vim registers" },
    R = { "<cmd>Telescope reloader<CR>",                  "Reload NeoLua module" },
    s = { "<cmd>lua luv_picker.skeletons()<CR>",          "Skeletons" },
    S = { "<cmd>Telescope symbols<CR>",                   "Symbols" },
    t = { "<cmd>Telescope treesitter<CR>",                "Treesitter" },
    T = { "<cmd>Telescope tags<CR>",                      "cTags" },
    v = { "<cmd>Telescope vim_options<CR>",               "Vim options" },
  },
  {
    mode    = "n",
    prefix  = "<C-c><C-c>",
    silent  = true,
    noremap = true,
    nowait  = false,
  }
)
wk.register(
  {
    name = "Telescope LSP",
    a = { "<cmd>Telescope lsp_code_actions<CR>",              "Code actions" },
    d = { "<cmd>Telescope lsp_document_diagnostics<CR>",      "Buffer diagnostics" },
    D = { "<cmd>Telescope lsp_workspace_diagnostics<CR>",     "Workspace diagnostics" },
    i = { "<cmd>Telescope lsp_implementations<CR>",           "Implementations" },
    n = { "<cmd>Telescope lsp_definitions<CR>",               "Definitions" },
    r = { "<cmd>Telescope lsp_references<CR>",                "References under cursor" },
    s = { "<cmd>Telescope lsp_document_symbols<CR>",          "Document symbols" },
    w = { "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", "Dynamic workspace symbols" },
    W = { "<cmd>Telescope lsp_workspace_symbols<CR>",         "Workspace symbols" },
  },
  {
    mode    = "n",
    prefix  = "<C-c><C-c><C-l>",
    silent  = true,
    noremap = true,
    nowait  = false,
  }
)
wk.register(
  {
    name = "Telescope Git",
    b = { "<cmd>Telescope git_branches<CR>", "Branches" },
    c = { "<cmd>Telescope git_commits<CR>",  "Commits" },
    C = { "<cmd>Telescope git_bcommits<CR>", "Buffer commits" },
    h = { "<cmd>Telescope git_stash<CR>",    "Stash" },
    s = { "<cmd>Telescope git_status<CR>",   "Status" },
  },
  {
    mode    = "n",
    prefix  = "<C-c><C-c><C-g>",
    silent  = true,
    noremap = true,
    nowait  = false,
  }
)
