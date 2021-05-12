require'bufferline'.setup{
  options = {
    max_name_length         = 18,
    max_prefix_length       = 15,
    tab_size                = 18,
    always_show_bufferline  = false,
    enforce_regular_tabs    = false,
    separator_style         = "slant",
    view                    = "multiwindow",
    numbers                 = "buffer_id",
    number_style            = "superscript",
    mappings                = false,
    buffer_close_icon       = '',
    modified_icon           = '●',
    close_icon              = '',
    left_trunc_marker       = '',
    right_trunc_marker      = '',
    show_buffer_close_icons = true,
    show_close_icon         = true,
    show_tab_indicators     = true,
    persist_buffer_sort     = true,
    sort_by                 =
      function(buf_a,buf_b)
        return vim.fn.bufnr(vim.fn.bufname(buf_a.id)) < vim.fn.bufnr(vim.fn.bufname(buf_b.id))
      end,
    custom_filter           =
      function(bufnr)
        if vim.bo[bufnr].filetype ~= "fugitive"
          and vim.bo[bufnr].filetype ~= 'qf'
        then
          return true
        end
      end,
    diagnostics             = "nvim_lsp",
    diagnostics_indicator   =
      function(_, _, diagnostics_dict)
        local s = " "
        for e, n in pairs(diagnostics_dict) do
          local sym = e == "error" and " "
            or (e == "warning" and " " or "" )
          s = s .. sym .. n .. " "
        end
        return s
      end,
  }
}
