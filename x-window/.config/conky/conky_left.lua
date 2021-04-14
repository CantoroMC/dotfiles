conky.config = {
  -- BEHAVIOUR: {{{1
  background                 = true,
  double_buffer              = true,
  extra_newline              = true,
  format_human_readable      = true,
  no_buffers                 = true,
  out_to_console             = false,
  out_to_ncurses             = false,
  out_to_stderr              = false,
  out_to_x                   = true,
  short_units                = false,
  temperature_unit           = celsius,
  times_in_seconds           = false,
  update_interval            = 1.0,
  update_interval_on_battery = 2.0,
  detect_battery             = 'BAT0',
  use_spacer                 = 'none',
  use_xft                    = true,
  cpu_avg_samples            = 4,
  -- diskio_avg_samples      = ,
  net_avg_samples            = 2,
  top_cpu_separate           = false,
  top_name_verbose           = false,
  top_name_width             = 15,
  -- }}}
  -- CONKY: {{{1
  alignment              = 'top_left',
  color0                 = '151a1e',
  color1                 = 'ff3333',
  color2                 = '88cc52',
  color3                 = 'eafe84',
  color4                 = '36a3d9',
  color5                 = 'f07178',
  color6                 = '95e6cb',
  color7                 = 'eaeaea',
  color8                 = '3f4e5a',
  default_color          = '#41a6d9',
  draw_blended           = true,
  font                   = 'Operator Mono Lig:size=10',
  font0                  = 'Operator Mono Lig:weight=bold:italic:pixelsize=14:antialias=true:hinting=true',
  font1                  = 'Operator Mono Lig:size=12',
  font2                  = 'Operator Mono Lig:size=9',
  font3                  = "SauceCodePro Nerd Font:style=Black Italic:size=9:hinting=true",
  font4                  = "mononoki Nerd Font:style=Regular:pixelsize=13",
  gap_x                  = 20,
  gap_y                  = 40,
  minimum_height         = 5,
  minimum_width          = 5,
  maximum_width          = 300,
  max_text_width         = 280,
  own_window             = true,
  own_window_class       = 'Conky',
  own_window_title       = 'conky',
  own_window_type        = 'override',
  own_window_colour      = '#151a1e',
  own_window_transparent = false,
  own_window_argb_visual = true,
  own_window_argb_value  = 175,
  pad_percents           = 0.9,
  lowercase              = false,
  uppercase              = false,
  -- }}}
  -- BORDERS: {{{1
  border_width        = 3,
  border_inner_margin = 5,
  border_outer_margin = 5,
  draw_borders        = false,
  stippled_borders    = 0,
  -- }}}
  -- OUTLINES: {{{1
  default_outline_color = '#151a1e',
  draw_outline          = false,
  -- }}}
  -- SHADES: {{{
  default_shade_color = '#323232',
  draw_shades         = true,
  -- }}}
  -- OBJECTS: {{{1
  -- BAR: {{{2
  console_bar_fill = '#',
  console_bar_unfill = '.',
  default_bar_height = 6,
  default_bar_width = 0,
  -- }}}
  -- GAUGE: {{{2
  default_gauge_height = 25,
  default_gauge_width = 40,
  -- }}}
  -- GRAPH: {{{2
  default_graph_height = 25,
  default_graph_width  = 0,
  draw_graph_borders   = false,
  show_graph_range     = false,
  show_graph_scale     = true,
  -- }}}
  -- }}}
}

-- TEXT: {{{1
conky.text = [[
${image ~/.config/conky/gentoo.png -p 5,2 -s 30x30 -f 86400}${font Operator Mono Lig:size=15}${alignc}$color7 Information$font$color
${offset 5}${voffset 10}${color gray}Machine: $color$alignr$nodename - ${addr wlan0}
${offset 5}${color gray}System: $color$alignr $sysname ($kernel)
${offset 150}$distribution
${offset 5}${color grey}Uptime: $color$alignr$uptime
${offset 5}${color grey}Disk Space $color $alignr / (${fs_type /}) ${fs_free /}/${fs_size /}
$alignr backup (${fs_type /mnt/backup}) ${fs_free /mnt/backup}/${fs_size /mnt/backup}
${offset 5}${color grey}Battery $color${color3}${alignr}${battery_bar 4,130 BAT0} ${battery_percent BAT0}%$color
$alignr (${battery_status BAT0} ${battery_time BAT0})
${color2}$font4  $font$font0 Calendar ${hr 2}$color$font
${alignc}$color2${font Operator Mono Lig:size=16}${time %I:%M:%S} - $font0${time %a %d %B %Y}$font$color
${voffset 10}${font2}$color7${execpi 1800 DA=`date +%_d`; cal -s -n 2 | sed s/"\(^\|[^0-9]\)$DA"'\b'/'\1${color2}'"$DA"'$color$color7'/}$font$color
$color1$font4  $font$font0 Temperatures ${hr 2}$color$font
${voffset 5}${offset 10}${color grey}Sensor 1$color $font4$font ( ${hwmon 3 fan 1} rpm)${goto 240} ${hwmon 4 temp 1} °C
${offset 10}${color grey}Sensor 2$color${goto 240} ${hwmon 4 temp 2} °C
${offset 10}${color grey}Sensor 3$color${goto 240} ${hwmon 4 temp 3} °C
${offset 10}${color grey}Sensor 4$color${goto 240} ${hwmon 4 temp 4} °C
${offset 10}${color grey}Sensor 5$color${goto 240} ${hwmon 4 temp 5} °C
${color4}$font4  $font$font0 Music ${hr 2}$color$font
${if_running cmus}${voffset 10}$font1 Cmus$font: $alignr $color7${scroll 40 $cmus_title - $cmus_artist}$color
$font2$color7$font3 $font $cmus_repeat $font3 $font $cmus_random$color$alignr$color3${cmus_progress 4,130}$color
$alignr$cmus_state: $cmus_curtime/$cmus_totaltime$endif
$if_mpd_playing $font1 Mpd$font: $alignr $color7${scroll 40 $mpd_title - $mpd_artist}$color
$font2$color7$font3 $font $mpd_repeat $font3 $font $mpd_random$color$alignr$color3${mpd_bar 4,130}$color
$alignr$mpd_status: $mpd_elapsed/$mpd_length $endif
]]
..
[[
${color4}$font4 ﱅ $font$font0 Note ${hr 2}$color$font
Configure conky and use it also as reminder application
]]

-- }}}
