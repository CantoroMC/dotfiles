local theme_assets  = require("beautiful.theme_assets")
local xresources    = require("beautiful.xresources")
local rnotification = require("ruled.notification")
local dpi           = xresources.apply_dpi
local theme_path    = require('beautiful').theme_path


local theme = {}

theme.font       = "Operator Mono Lig 8"
theme.wallpaper  = theme_path.."/wallpapers/zenburn.png"
theme.icon_theme = "Papirus" -- if `nil` icon from /usr/share/icons and /usr/share/icons/hicolor

-- Colors {{{1
theme.bg_normal           = "#151a1e"
theme.bg_focus            = "#232b32"
theme.bg_urgent           = "#ff3333"
theme.bg_minimize         = "#3f4e5a"
theme.bg_systray          = "#232b32"
theme.fg_normal           = "#fafafa"
theme.fg_focus            = "#b8cc52"
theme.fg_urgent           = "#e7c547"
theme.fg_minimize         = "#eaeaea"
theme.useless_gap         = dpi(2)
theme.border_width        = dpi(1)
theme.border_color_normal = "#000000"
theme.border_color_active = "#535d6c"
theme.border_color_marked = "#91231c"
-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- }}}

-- Notifications: {{{1
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]
-- }}}

-- Menu: {{{1
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = theme_path.."submenu.png"
theme.menu_height = dpi(18)
theme.menu_width  = dpi(120)
theme.awesome_icon = theme_path .. "awesome-icon.png"
-- }}}

-- Widgets {{{1
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.fg_widget        = "#AECF96"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
-- }}}

-- Titlebar Icons: {{{1

-- Taglist squares: {{{2
theme.taglist_squares_sel =
  theme_assets.taglist_squares_sel(dpi(5), theme.fg_focus)
theme.taglist_squares_unsel =
  theme_assets.taglist_squares_unsel(dpi(5), theme.fg_normal)
-- }}}

-- Titlebar {{{2
theme.titlebar_close_button_normal              = theme_path .. "titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme_path .. "titlebar/close_focus.png"
theme.titlebar_minimize_button_normal           = theme_path .. "titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme_path .. "titlebar/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme_path .. "titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme_path .. "titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme_path .. "titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme_path .. "titlebar/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme_path .. "titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme_path .. "titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme_path .. "titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme_path .. "titlebar/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme_path .. "titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme_path .. "titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme_path .. "titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme_path .. "titlebar/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme_path .. "titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme_path .. "titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme_path .. "titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme_path .. "titlebar/maximized_focus_active.png"
-- }}}

-- }}}

-- Layouts Icons: {{{1
theme.layout_tile       = theme_path .. "layouts/tile.png"
theme.layout_tileleft   = theme_path .. "layouts/tileleft.png"
theme.layout_fairh      = theme_path .. "layouts/fairh.png"
theme.layout_fairv      = theme_path .. "layouts/fairv.png"
theme.layout_floating   = theme_path .. "layouts/floating.png"
theme.layout_magnifier  = theme_path .. "layouts/magnifier.png"
theme.layout_max        = theme_path .. "layouts/max.png"
theme.layout_fullscreen = theme_path .. "layouts/fullscreen.png"
theme.layout_tilebottom = theme_path .. "layouts/tilebottom.png"
theme.layout_tiletop    = theme_path .. "layouts/tiletop.png"
theme.layout_spiral     = theme_path .. "layouts/spiral.png"
theme.layout_dwindle    = theme_path .. "layouts/dwindle.png"
theme.layout_cornernw   = theme_path .. "layouts/cornernw.png"
theme.layout_cornerne   = theme_path .. "layouts/cornerne.png"
theme.layout_cornersw   = theme_path .. "layouts/cornersw.png"
theme.layout_cornerse   = theme_path .. "layouts/cornerse.png"
-- }}}

-- Connect urgent notifications {{{1
rnotification.connect_signal('request::rules', function()
  rnotification.append_rule {
    rule       = { urgency = 'critical' },
    properties = { bg = theme.bg_urgent, fg = theme.fg_urgent }
  }
end)
-- }}}

return theme

-- vim:fdm=marker
