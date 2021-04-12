local wibox   = require("wibox")
local naughty = require("naughty")
local gfs     = require("gears.filesystem")

local last_id
widget = wibox.widget.textbox()
function testtemps()
  local fd = io.popen(gfs.get_configuration_dir() .. "/widgets/temperature/sensors.sh")
  if fd then
    local tempstr = fd:read("*all")
    local temp = tonumber(tempstr)
    fd:close()
    local color
    local font = "<span font='FontAwesome 8' "
    if temp > 90 then
      color = font .. "color='#FF0000'>"
      last_id = naughty.notify({
          title       = "Temperature Critical",
          text        = "CPU temperature is dangerously hot, turn it off to prevent damage.",
          preset      = naughty.config.presets.critical,
          replaces_id = last_id,
          icon        = gfs.get_configuration_dir() .. "/widgets/temperature/" .. "critical.png"
      }).id
    elseif temp > 80 then
      color = font .. "color='#FF8000'>"
    elseif temp > 70 then
      color = font .. "color='#F5F549'>"
    else
      color = font .. "color='#669900'>"
    end

    if widget.zenstate ~= nil then
      if widget.zenstate(temp) then
        return ""
      end
    end

    return color .. " " .. temp .. " </span>"
  end
  return " N/A " -- something failed
end

widget:set_markup(testtemps())

-- update every 30 secs
temptimer = timer({ timeout = 30 })
temptimer:connect_signal("timeout", function() widget:set_markup(testtemps()) end)
temptimer:start()
