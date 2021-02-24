local keyword_prg = function(word)
    local original_iskeyword = vim.bo.iskeyword

    vim.bo.iskeyword = vim.bo.iskeyword .. ',.'

    word = word or vim.fn.expand("<cword>")

    vim.bo.iskeyword = original_iskeyword

    -- TODO: This is kind of a lame hack... since you could rename `vim.api` -> `a` or similar
    if string.find(word, 'vim.api') then
      local _, finish = string.find(word, 'vim.api.')
      local api_function = string.sub(word, finish + 1)

      vim.cmd(string.format('help %s', api_function))
      return
    elseif string.find(word, 'vim.fn') then
      local _, finish = string.find(word, 'vim.fn.')
      local api_function = string.sub(word, finish + 1) .. '()'

      vim.cmd(string.format('help %s', api_function))
      return
    else
      -- TODO: This should be exact match only. Not sure how to do that with `:help`
      -- TODO: Let users determine how magical they want the help finding to be
      local ok = pcall(vim.cmd, string.format('help %s', word))

      if not ok then
        local split_word = vim.split(word, '.', true)
        ok = pcall(vim.cmd, string.format('help %s', split_word[#split_word]))
      end

      if not ok then
        vim.lsp.buf.hover()
      end
    end
end

return {
  keyword_prg = keyword_prg
}
