local compe = require'compe'
local Source = {}

local split = function(inputstr, sep)
  if sep == nil then
    sep = "%s"
  end
  local t={}
  for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
    table.insert(t, str)
  end
  return t
end

function Source.new()
  return setmetatable({}, { __index = Source })
end

function Source.get_metadata(self)
  return {
    dup = false;
    kind = "Email";
    menu = " ";
    priority = 1;
    sort = false;
  }
end

function Source.determine(self, context)
  return compe.helper.determine(context)
end

function Source.complete(self, context)
  local mails = vim.fn.systemlist("grep email $ADDRESSES")
  local mail = {}
  for k,v in pairs(mails) do
    mail[k] = string.gsub(v,'email=','')

    if mail[k]:find(',') then
      exp = split(mail[k], ',')
      table.remove(mail,k)
      for _,v in ipairs(exp) do
        table.insert(mail, v)
      end
    end
  end
  context.callback({items = mail})
end

return Source
