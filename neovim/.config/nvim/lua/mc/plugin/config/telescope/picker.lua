-- local actions = require('telescope.actions')
local actions = require("telescope.actions")
local action_set = require('telescope.actions.set')
local action_state = require('telescope.actions.state')

local set_prompt_to_entry_value = function(prompt_bufnr)
  local entry = action_state.get_selected_entry()
  if not entry or not type(entry) == 'table' then return end

  action_state.get_current_picker(prompt_bufnr):reset_prompt(entry.ordinal)
end

local M = {}

function M.dotfiles()
  require('telescope.builtin').find_files {
    prompt_title = "Dotfiles",
    shorten_path = false,
    cwd = "~/dotfiles",
    find_command = {
      'rg',
      '--hidden',
      '--files',
      '--glob=!.git/*',
      '-l',
    },

    attach_mappings = function(_, map)
      map('i', '<c-y>', set_prompt_to_entry_value)

      return true
    end,
  }
end

function M.neovim_config()
  require('telescope.builtin').find_files {
    prompt_title = "Neovim Config",
    shorten_path = false,
    cwd = "~/dotfiles/neovim",
    find_command = {
      'rg',
      '--hidden',
      '--files',
      '--glob=!.git/*',
      '-l',
    },

    attach_mappings = function(_, map)
      map('i', '<c-y>', set_prompt_to_entry_value)

      return true
    end,
  }
end

function M.skeletons()
  require'telescope.builtin'.find_files {
    prompt_title = 'Skeletons',
    shorten_path = false,
    cwd = vim.fn.stdpath('config').."/data/skeletons",
    attach_mappings = function(prompt_bufnr)
      action_set.select:replace(
        function(prompt_bufnr, type)
          local entry = action_state.get_selected_entry()
          actions.close(prompt_bufnr)
          vim.cmd('-1read '..entry.cwd..'/'..entry.filename)
        end)
        return true
    end,
  }
end

return M
