local M = {}

local plug_settings = {
  ["true"]  = "false",
  ["True"]  = "False",
  ["1"]     = "0",
  ["Yes"]   = "No",
  ["yes"]   = "no",
  ["right"] = "left",
  ["pre"]   = "post",
}

plug_settings = vim.tbl_add_reverse_lookup(plug_settings)

local function errorHandler(err)
  if not err == nil then
    print("Error toggling to alternate value. Err: "..err)
  end
end

function M.toggleAlternate(str)
  if plug_settings[str] == nil then
    print("Unsupported alternate value.")
    return
  end

  xpcall(vim.cmd('normal ciw'..plug_settings[str]), errorHandler)
end

function M.setup(user_settings)
  if user_settings then
    user_settings = vim.tbl_add_reverse_lookup(user_settings)
    plug_settings = vim.tbl_extend("force", plug_settings, user_settings)
  end
end

return M
