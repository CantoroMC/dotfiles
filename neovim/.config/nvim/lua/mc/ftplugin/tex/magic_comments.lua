-- TeXmagic.nvim (Magic comment finder for Neovim)
local M = {}

local config_defaults = {
  engines = {
    pdflatex = {
      executable = "latexmk",
      args = {
        "-pdflatex",
        "-shell-escape",
        "-interaction=nonstopmode",
        "-synctex=1",
        "-file-line-error",
        "%f"
      },
      onSave = false,
      forwardSearchAfter = false,
    },
    lualatex = {
      executable = "latexmk",
      args = {
        "-lualatex",
        "-shell-escape",
        "-interaction=nonstopmode",
        "-synctex=1",
        "-file-line-error",
        "%f"
      },
      onSave = false,
      forwardSearchAfter = false,
    },
    xelatex = {
      executable = "latexmk",
      args = {
        "-xelatex",
        "-shell-escape",
        "-interaction=nonstopmode",
        "-synctex=1",
        "-file-line-error",
        "%f"
      },
      onSave = false,
      forwardSearchAfter = false,
    },
    dvipspdf = {
      executable = "latexmk",
      args = {
        "-dvi",
        "-ps",
        "-pdfps",
        "-shell-escape",
        "-interaction=nonstopmode",
        "-synctex=1",
        "-file-line-error",
        "%f"
      },
      onSave = false,
      forwardSearchAfter = false,
    },
  }
}

function M.find_magic_comments(path)
  local file = io.open(path, "r")
  if file ~= nil then
    local line = file:read()
    local magic = {}
    if line ~= nil then
      while string.find(line, '%%%s*!%s*TeX') ~= nil do
        table.insert(magic, line)
        line = file:read()
        if line == nil then
          line = ""
        end
      end
    end
    file:close()
    return(magic)
  else
    return {}
  end
end

function M.program(magic)
  if #magic == 0 then
    return nil
  else
    local program
    for i = 1, #magic, 1 do
      if string.find(magic[i], "program") ~= nil then
        local start = string.find(magic[i], "=") + 1
        program = string.lower(string.gsub(string.sub(magic[i], start), "%s+", ""))
      end
    end
    return(program)
  end
end

function M.setup(user_config)
  local buf_name = vim.api.nvim_buf_get_name(0)
  local program = M.program(M.find_magic_comments(buf_name))
  if user_config then
    config_defaults = vim.tbl_deep_extend("force", config_defaults, user_config)
  end
  return program and
    config_defaults.engines[program] or
    config_defaults.engines.pdflatex
end

return M
