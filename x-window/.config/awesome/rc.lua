-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- this is to trick the LSP
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag

local awful         = require("awful")               -- Standard awesome library
local beautiful     = require("beautiful")           -- Theme handling library
local freedesktop   = require("freedesktop")         -- freedesktop complaiant menu
local gears         = require("gears")               -- Standard awesome library
local hotkeys_popup = require("awful.hotkeys_popup") -- Declarative object management
local menubar       = require("menubar")
local naughty       = require("naughty")             -- Notification library
local ruled         = require("ruled")
local wibox         = require("wibox")               -- Widget and layout library

local calendar = require("widgets.calendar")
local battery  = require("widgets.battery")

require("awful.autofocus")
-- require("awful.hotkeys_popup.keys")


function load_zen_widget(config)
  require(config.widget)

  -- if user defined a zenstate, assign it to the widget so it can respond accordingly
  if config.zenstate then
    widget.zenstate = config.zenstate
  end

  -- notify that the widget was loaded, if asked by rc.lua
  if config.notify then
    naughty.notify({
      title = "Widget Loaded",
      text = "Loaded " .. config.widget
    })
  end

  return widget
end


-- ERROR_HANDLING: {{{
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
naughty.connect_signal(
  "request::display_error", function(message, startup)
    naughty.notification {
      urgency = "critical",
      title   = "Oops, an error happened" .. (startup and " during startup!" or "!"),
      message = message,
    }
end)
-- }}}

-- VARIABLES: {{{
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/ayu/theme.lua")
terminal   = "kitty"
editor     = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor
modkey     = "Mod4"
-- }}}

-- MENU: {{{
menubar.utils.terminal = terminal

AwesomeMenu = {
  { "hotkeys", function()
      hotkeys_popup.show_help(nil, awful.screen.focused())
    end },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end },
}

MainMenu = freedesktop.menu.build({
  before = {
    { "Awesome", AwesomeMenu, beautiful.awesome_icon },
  },
  after = {
    { "open terminal", terminal },
  }
})

Launcher = awful.widget.launcher({
  image = beautiful.awesome_icon,
  menu = MainMenu
})


-- }}}

-- LAYOUTS: {{{
-- Existing (tile, tile.{left,bottom,top}, fair, fair.horizontal,
--           spiral, spiral.dwindle, corner.{nw,ne,sw,se},
--           magnifier, max, max.fullscreen, floating)
tag.connect_signal("request::default_layouts", function()
  awful.layout.append_default_layouts({
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    awful.layout.suit.corner.ne,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.corner.se,
    -- awful.layout.suit.corner.sw,
  })
end)
-- }}}

-- WIBAR: {{{
TextClock = wibox.widget.textclock()
local Calendar = calendar({
  placement = 'top_right',

})
TextClock:connect_signal(
  "button::press",
  function(_, _, _, button)
    if button == 1 then Calendar.toggle() end
  end
)

TempWidget = load_zen_widget({
  widget = "widgets.temperature",
	zenstate = function(t)
    if t < 50 then
      return true
    end
    return false
  end,
})

screen.connect_signal("request::wallpaper", function(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end)

screen.connect_signal("request::desktop_decoration", function(s)
  -- Each screen has its own tag table.
  awful.tag(
    { "𑇡", "𑇢", "𑇣", "𑇤", "𑇥", "𑇦", "𑇧", "𑇨", "𑇩" },
    s,
    { awful.layout.suit.tile,
      awful.layout.suit.corner.nw,
      awful.layout.suit.tile.left,
      awful.layout.suit.magnifier,
      awful.layout.suit.fair,
      awful.layout.suit.tile,
      awful.layout.suit.tile.bottom,
      awful.layout.suit.max.fullscreen,
      awful.layout.suit.floating,
    }
  )

  s.PromptBox = awful.widget.prompt()

  s.LayoutBox = awful.widget.layoutbox {
    screen  = s,
    buttons = {
      awful.button({ }, 1, function () awful.layout.inc( 1) end),
      awful.button({ }, 3, function () awful.layout.inc(-1) end),
      awful.button({ }, 4, function () awful.layout.inc(-1) end),
      awful.button({ }, 5, function () awful.layout.inc( 1) end),
    }
  }

  s.TagList = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = {
      awful.button({ }, 1, function(t)
        t:view_only()
      end),
      awful.button({ modkey }, 1, function(t)
        if client.focus then
          client.focus:move_to_tag(t)
        end
      end),
      awful.button({ }, 3, awful.tag.viewtoggle),
      awful.button({ modkey }, 3, function(t)
        if client.focus then
          client.focus:toggle_tag(t)
        end
      end),
      awful.button({ }, 4, function(t)
        awful.tag.viewprev(t.screen)
      end),
      awful.button({ }, 5, function(t)
        awful.tag.viewnext(t.screen)
      end),
    }
  }

  -- Create a tasklist widget
  s.TaskList = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = {
      awful.button({ }, 1, function (c)
        c:activate {
          context = "tasklist",
          action = "toggle_minimization" }
      end),
      awful.button({ }, 2, function (c)
        c:kill()
      end),
      awful.button({ }, 3, function()
        awful.menu.client_list {
          theme = { width = 250 }
        }
      end),
      awful.button({ }, 4, function()
        awful.client.focus.byidx(-1)
      end),
      awful.button({ }, 5, function()
        awful.client.focus.byidx( 1)
      end),
    }
  }

  s.WiBox = awful.wibar({
    position = "top",
    screen = s
  })

  s.WiBox.widget = {
    layout = wibox.layout.align.horizontal,
    {
      layout = wibox.layout.fixed.horizontal,
      Launcher,
      s.TagList,
      s.LayoutBox,
    },
    s.TaskList,
    {
      layout = wibox.layout.fixed.horizontal,
      TempWidget,
      s.PromptBox,
      wibox.widget.systray(),
      battery(),
      TextClock,
    },
  }
end)
-- }}}

-- MOUSE_BINDINGS_AND_KEY_BINDINGS: {{{1

-- MOUSE: {{{2
awful.mouse.append_global_mousebindings({
  awful.button({ }, 3, function () MainMenu:toggle() end),
  awful.button({ }, 4, awful.tag.viewprev),
  awful.button({ }, 5, awful.tag.viewnext),
})
-- }}}

-- KEY: {{{2
awful.keyboard.append_global_keybindings({
  -- Left Side:
  awful.key(
    { modkey, }, "q",
    awesome.restart,
    { description = "reload awesome",
      group = "awesome"
    }
  ),
  awful.key(
    { modkey, },"w",
    function ()
      awful.screen.focus_relative(-1)
    end,
    { description = "focus the previous screen",
      group = "screen"
    }
  ),
  awful.key(
    { modkey, }, "e",
    function ()
      awful.screen.focus_relative( 1)
    end,
    { description = "focus the next screen",
      group = "screen"
    }
  ),
  awful.key(
    { modkey, }, "r",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
    end,
    { description = "go back",
      group = "client"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "r",
    awful.tag.history.restore,
    { description = "go back",
      group = "tag"
    }
  ),
  awful.key(
    { modkey, }, "s",
    function ()
      awful.tag.incgap(1)
    end,
    { description = "increase gap",
      group = "tag"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "s",
    function ()
      awful.tag.incgap(-1)
    end,
    { description = "decrease gap",
      group = "tag"
    }
  ),
  awful.key(
    { modkey, }, "f",
    function ()
      awful.spawn("vivaldi-stable")
    end,
    { description = "open the vivaldi browser",
      group = "launcher"
    }
  ),
  awful.key(
    { modkey }, "x",
    function ()
      awful.prompt.run{
        prompt       = "Run Lua code: ",
        textbox      = awful.screen.focused().PromptBox.widget,
        exe_callback = awful.util.eval,
        history_path = awful.util.get_cache_dir() .. "/history_eval"
      }
    end,
    { description = "lua execute prompt",
      group = "awesome"
    }
  ),
  awful.key(
    { modkey, }, "v",
    awful.client.urgent.jumpto,
    { description = "jump to urgent client",
      group = "client"
    }
  ),
  awful.key(
    { modkey }, "b",
    function ()
      for s in screen do
        s.WiBox.visible = not s.WiBox.visible
        if s.wiboxHided then
          s.wiboxHided.visible = not s.wiboxHided.visible
        end
      end
    end,
    { description = "toggle wibox", group = "awesome" }
  ),
  -----------------------------------------------------------------------------
  -- Right Side:
  awful.key(
    { modkey }, "u",
    function ()
      awful.screen.focused().PromptBox:run()
    end,
    { description = "run prompt",
      group = "launcher"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "u",
    function ()
      MainMenu:show()
    end,
    { description = "show main menu",
      group = "launcher"
    }
  ),
  awful.key(
    { modkey, "Control" }, "u",
    function()
      menubar.show()
    end,
    { description = "show the menubar",
      group = "launcher"
    }
  ),
  awful.key(
    { modkey, }, "j",
    function ()
      awful.client.focus.byidx( 1)
    end,
    { description = "focus next by index",
      group = "client"
    }
  ),
  awful.key(
    { modkey, }, "k",
    function ()
      awful.client.focus.byidx(-1)
    end,
    { description = "focus previous by index",
      group = "client"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "j",
    function ()
      awful.client.swap.byidx(  1)
    end,
    { description = "swap with next client by index",
      group = "client"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "k",
    function ()
      awful.client.swap.byidx( -1)
    end,
    { description = "swap with previous client by index",
      group = "client"
    }
  ),
  awful.key(
    { modkey, }, "h",
    function ()
      awful.tag.incmwfact(-0.05)
    end,
    { description = "decrease master width factor",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, }, "l",
    function ()
      awful.tag.incmwfact( 0.05)
    end,
    { description = "increase master width factor",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "n",
    function ()
      local c = awful.client.restore()
      if c then
        c:activate {
          raise = true,
          context = "key.unminimize"
        }
      end
    end,
    { description = "restore minimized",
      group = "client"
    }
  ),
  -----------------------------------------------------------------------------
  -- Extra Keys:
  awful.key(
    { modkey, }, "Tab",
    function ()
      awful.layout.inc( 1)
    end,
    { description = "select next",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "Tab",
    function ()
      awful.layout.inc(-1)
    end,
    { description = "select previous",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "BackSpace",
    function ()
      awful.spawn("slock")
    end,
    { description = "screen locker",
      group = "utilities"
    }
  ),
  awful.key(
    { modkey, "Shift" }, "Delete",
    awesome.quit,
    { description = "quit awesome",
      group = "awesome"
    }
  ),
  awful.key(
    { modkey, }, "Return",
    function ()
      awful.spawn(terminal)
    end,
    { description = "open a terminal",
      group = "launcher"
    }
  ),
  awful.key(
    { modkey, }, "[",
    awful.tag.viewprev,
    { description = "view previous",
      group = "tag"
    }
  ),
  awful.key(
    { modkey, }, "]",
    awful.tag.viewnext,
    { description = "view next",
      group = "tag"
    }
  ),
  awful.key(
    { modkey, }, ",",
    function ()
      awful.tag.incnmaster( 1, nil, true)
    end,
    { description = "increase the number of master clients",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, }, ".",
    function ()
      awful.tag.incnmaster(-1, nil, true)
    end,
    { description = "decrease the number of master clients",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, "Shift" }, ",",
    function ()
      awful.tag.incncol( 1, nil, true)
    end,
    { description = "increase the number of columns",
      group = "layout"
    }
  ),
  awful.key(
    { modkey, "Shift" }, ".",
    function ()
      awful.tag.incncol(-1, nil, true)
    end,
    { description = "decrease the number of columns",
      group = "layout"
    }
  ),
  -----------------------------------------------------------------------------
  -- Arrows:
  -----------------------------------------------------------------------------
  -- Numbers
  awful.key {
    modifiers   = { modkey },
    keygroup    = "numrow",
    description = "only view tag",
    group       = "tag",
    on_press    = function (index)
      local screen = awful.screen.focused()
      local tag = screen.tags[index]
      if tag then
        tag:view_only()
      end
    end,
  },
  awful.key {
    modifiers = { modkey, "Shift" },
    keygroup    = "numrow",
    description = "move focused client to tag",
    group       = "tag",
    on_press    = function (index)
      if client.focus then
        local tag = client.focus.screen.tags[index]
        if tag then
          client.focus:move_to_tag(tag)
        end
      end
    end,
  },
  awful.key {
    modifiers   = { modkey, "Control" },
    keygroup    = "numrow",
    description = "toggle tag",
    group       = "tag",
    on_press    = function (index)
      local screen = awful.screen.focused()
      local tag = screen.tags[index]
      if tag then
        awful.tag.viewtoggle(tag)
      end
    end,
  },
  awful.key {
    modifiers   = { modkey, "Control", "Shift" },
    keygroup    = "numrow",
    description = "toggle focused client on tag",
    group       = "tag",
    on_press    = function (index)
      if client.focus then
        local tag = client.focus.screen.tags[index]
        if tag then
          client.focus:toggle_tag(tag)
        end
      end
    end,
  },
  awful.key {
    modifiers   = { modkey },
    keygroup    = "numpad",
    description = "select layout directly",
    group       = "layout",
    on_press    = function (index)
      local t = awful.screen.focused().selected_tag
      if t then
        t.layout = t.layouts[index] or t.layout
      end
    end,
  },
  -----------------------------------------------------------------------------
  -- Fn and Xf86 keys
  awful.key(
    { modkey, }, "F1",
    hotkeys_popup.show_help,
    { description="show help",
      group="awesome"
    }
  ),
  awful.key(
    { }, "XF86AudioMute",
    function ()
      awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")
    end,
    { description = "Mute the sound",
      group = "utilities"
    }
  ),
  awful.key(
    { }, "XF86AudioLowerVolume",
    function ()
      awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%")
    end,
    { description = "-5% volume",
      group = "utilities"
    }
  ),
  awful.key(
    { }, "XF86AudioRaiseVolume",
    function ()
      awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%")
    end,
    { description = "+5% volume",
      group = "utilities"
    }
  ),
  awful.key(
    { }, "XF86MonBrightnessDown",
    function ()
      awful.spawn("xbacklight -dec 5")
    end,
    { description = "-5% brightness",
      group = "utilities"
    }
  ),
  awful.key(
    { }, "XF86MonBrightnessUp",
    function ()
      awful.spawn("xbacklight -inc 5")
    end,
    { description = "+5% brightness",
      group = "utilities"
    }
  ),
})
-- }}}

-- CLIENT_MOUSE: {{{2
client.connect_signal("request::default_mousebindings", function()
  awful.mouse.append_client_mousebindings({
    awful.button({ }, 1, function (c)
      c:activate { context = "mouse_click" }
    end),
    awful.button({ modkey }, 1, function (c)
      c:activate { context = "mouse_click", action = "mouse_move"  }
    end),
    awful.button({ modkey }, 3, function (c)
      c:activate { context = "mouse_click", action = "mouse_resize" }
    end),
  })
end)
-- }}}

-- CLIENT_KEYS: {{{2
client.connect_signal("request::default_keybindings",
  function()
    awful.keyboard.append_client_keybindings({
      -- Left Side:
      awful.key(
        { modkey, "Shift" }, "q",
        function (c)
          c:kill()
        end,
        { description = "close",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Shift"}, "w",
        function (c)
          c:move_to_screen(c.screen.index - 1)
        end,
        { description = "move to screen",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Shift"}, "e",
        function (c)
          c:move_to_screen(c.screen.index + 1)
        end,
        { description = "move to screen",
          group = "client"
        }
      ),
      awful.key(
        { modkey, }, "t",
        awful.client.floating.toggle,
        { description = "toggle floating",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Control" }, "t",
        function (c)
          c.ontop = not c.ontop
        end,
        { description = "toggle keep on top",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Control" }, "f",
        function (c)
          c.fullscreen = not c.fullscreen
          c:raise()
        end,
        { description = "toggle fullscreen",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Control" }, "v",
        function (c)
          c.maximized_vertical = not c.maximized_vertical
          c:raise()
        end ,
        { description = "(un)maximize vertically",
          group = "client"
        }
      ),
      -------------------------------------------------------------------------
      -- Right Side
      awful.key(
        { modkey, "Control" }, "b",
        function (c)
          c.maximized_horizontal = not c.maximized_horizontal
          c:raise()
        end ,
        { description = "(un)maximize horizontally",
          group = "client"
        }
      ),
      awful.key(
        { modkey, }, "n",
        function (c)
          c.minimized = true
        end,
        { description = "minimize",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Control" }, "n",
        function (c)
          c.maximized = not c.maximized
          c:raise()
        end ,
        { description = "(un)maximize",
          group = "client"
        }
      ),
      awful.key(
        { modkey, "Shift" }, "m",
        function (c)
          c:swap(awful.client.getmaster())
        end,
        { description = "move to master",
          group = "client"
        }
      ),
    })
  end)
-- }}}

-- }}}

-- RULES: {{{
ruled.client.connect_signal("request::rules", function()
  -- All clients will match this rule.
  ruled.client.append_rule {
    id         = "global",
    rule       = { },
    properties = {
      focus     = awful.client.focus.filter,
      raise     = true,
      screen    = awful.screen.preferred,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  }

  -- Floating clients.
  ruled.client.append_rule {
    id       = "floating",
    rule_any = {
      instance = { "copyq", "pinentry" },
      class    = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "Sxiv",
        "Tor Browser",
        "Wpa_gui",
        "veromix",
        "xtightvncviewer"
      },
      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name    = {
        "Event Tester",  -- xev.
      },
      role    = {
        "AlarmWindow",    -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
        "pop-up",         -- e.g. Google Chrome's (detached) Developer Tools.
      }
    },
    properties = { floating = true }
  }

  -- Add titlebars to normal clients and dialogs
  ruled.client.append_rule {
    id         = "titlebars",
    rule_any   = { type = { "normal", "dialog" } },
    properties = { titlebars_enabled = true      }
  }

  -- Set Firefox to always map on the tag named "2" on screen 1.
  -- ruled.client.append_rule {
  --   rule       = { class = "Firefox"     },
  --   properties = { screen = 1, tag = "2" }
  -- }
end)

-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal("arrange", function (s)
  local only_one = #s.tiled_clients == 1
  for _, c in pairs(s.clients) do
    if only_one and not c.floating or c.maximized then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
end)

-- }}}

-- TITLEBARS: {{{
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    awful.button(
      { }, 1,
      function()
        c:activate { context = "titlebar", action = "mouse_move"  }
      end
    ),
    awful.button(
      { }, 3,
      function()
        c:activate { context = "titlebar", action = "mouse_resize"}
      end
    ),
  }

  awful.titlebar(c).widget = {
    layout = wibox.layout.align.horizontal,
    { -- Left
      awful.titlebar.widget.iconwidget  (c),
      awful.titlebar.widget.stickybutton(c),
      awful.titlebar.widget.ontopbutton (c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      awful.titlebar.widget.floatingbutton (c),
      awful.titlebar.widget.minimizebutton (c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.closebutton    (c),
      layout = wibox.layout.fixed.horizontal()
    },
  }
end)
-- }}}

-- NOTIFICATIONS: {{{
ruled.notification.connect_signal('request::rules', function()
  -- All notifications will match this rule.
  ruled.notification.append_rule {
    rule       = { },
    properties = {
      screen           = awful.screen.preferred,
      implicit_timeout = 5,
    }
  }
end)

naughty.connect_signal("request::display",
  function(n)
    naughty.layout.box { notification = n }
  end
)
-- }}}

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:activate { context = "mouse_enter", raise = false }
-- end)

-- vim:fdm=marker

