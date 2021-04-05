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
  alignment              = 'bottom_right',
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
${color1}$font4  $font$font0 Cpu ${hr 2}$color$font
${voffset 10}${offset 4}$color7 Average  ${color1}${cpubar cpu0 6,190}$color ${cpu cpu0}%
${offset 10}Core 1   ${color1}${cpubar cpu1 3,110}$color ${cpu cpu1}% (${freq 1} MHz)
${offset 10}Core 2   ${color1}${cpubar cpu2 3,110}$color ${cpu cpu2}% (${freq 2} MHz)
${offset 10}Core 3   ${color1}${cpubar cpu3 3,110}$color ${cpu cpu3}% (${freq 3} MHz)
${offset 10}Core 4   ${color1}${cpubar cpu4 3,110}$color ${cpu cpu4}% (${freq 4} MHz)
${offset 10}Core 5   ${color1}${cpubar cpu5 3,110}$color ${cpu cpu5}% (${freq 5} MHz)
${offset 10}Core 6   ${color1}${cpubar cpu6 3,110}$color ${cpu cpu6}% (${freq 6} MHz)
${offset 10}Core 7   ${color1}${cpubar cpu7 3,110}$color ${cpu cpu7}% (${freq 7} MHz)
${offset 10}Core 8   ${color1}${cpubar cpu8 3,110}$color ${cpu cpu8}% (${freq 8} MHz)
${offset 4}${cpugraph cpu0 b8cc52 ff7733 -t}
$color2$font4  $font$font0 Memory ${hr 2}$color$font
${voffset 5}${color grey}RAM Usage:$color $alignr$mem/$memmax ($memperc%)
${color grey}Swap Usage:$color $alignr$swap/$swapmax ($swapperc%)
$color2$font4  $font$font0 Disk ${hr 2}$color$font
${voffset 5}${offset 15}$color2 Read (${diskio_read /dev/sdb}) ${offset 70} Write (${diskio_write /dev/sdb})$color
${diskiograph_read /dev/sdb 32,150 f0000f 0f0f00} $alignr ${diskiograph_write /dev/sdb 32,150 f0000f 0f0f00}
$color7$font4  $font$font0 Networking ${hr 2}$color$font
${voffset 10}${downspeedgraph enp0s25 32,120 ff0000 0000ff}$alignr${upspeedgraph enp0s25 32,120 0000ff ff0000}
$font3 ▼ $font${downspeed enp0s25}${alignc}$color2 IPv4 $color $alignr$font3▲ $font ${upspeed enp0s25}
${downspeedgraph wlan0 32,120 ff0000 0000ff}$alignr${upspeedgraph wlan0 32,120 0000ff ff0000}
$font3 ▼ $font${downspeed wlan0}${alignc}$color2 ${wireless_essid wlan0}$color ${alignr}$font3▲ $font${upspeed wlan0}
$color3$font4 省 $font$font0 Processes ${hr 2}$color$font
${voffset 5}$alignc${color grey}Processes:$color $processes  ${color grey}Running:$color $running_processes
${color grey} Name              PID     CPU%   MEM%$color
${top name 1} ${top pid 1} ${top cpu 1} ${top mem 1}
${top name 2} ${top pid 2} ${top cpu 2} ${top mem 2}
${top name 3} ${top pid 3} ${top cpu 3} ${top mem 3}
${top name 4} ${top pid 4} ${top cpu 4} ${top mem 4}

]]
-- }}}

-- vim:fdm=marker
