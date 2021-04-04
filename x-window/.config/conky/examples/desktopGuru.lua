conky.config ={
  -- CONKY SETTINGS
  background = true,
  update_interval = 1,
  total_run_times = 0,

  cpu_avg_samples = 2,
  net_avg_samples = 2,

  override_utf8_locale = true,

  double_buffer = true,
  no_buffers = true,

  text_buffer_size = 32768,

  -- CONKY: WINDOW SPECIFICATIONS
  own_window = true,
  own_window_argb_value = 192,
  own_window_argb_visual = true,
  own_window_class = 'conky-semi',
  own_window_colour = '#0b0d18',
  own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
  -- own_window_transparent = true,
  own_window_transparent = false,
  own_window_type = 'normal',
  -- own_window_type = 'desktop',

  border_inner_margin = 6,
  border_width = 5, 

  minimum_height = 1020,

  minimum_width = 280,
  maximum_width = 280,

  alignment = 'top_right',
  gap_x = 10,
  gap_y = 15,

  -- GRAPHICS SETTINGS
  draw_shades = false,
  draw_outline = true,
  draw_borders = false,
  draw_graph_borders = true,
  -- draw_graph_borders = false,

  -- TEXT SETTINGS
  use_xft = true,
  font = 'Montserrat Regular:size=10',
  xftalpha = 0.2,

  uppercase = false,

  -- Pad percentages to this many decimals (0 = no padding)
  pad_percents = 2,

  -- COLOUR SETTINGS
  default_color = 'AAAAAA',
  default_shade_color = '333333',
  -- default_outline_color = '111111',
  default_outline_color = '010101',
  color1 = '999999',
  color2 = 'CCCCCC',
  color3 = 'EEEEEE',
  color4 = '444444',
  color5 = '555555',
  color6 = '666666',
  color7 = '3b71a1',
};


conky.text = [[
${alignc}${color1}${font Montserrat Bold:size=20}${time %H:%M:%S}${font}${color}${alignc}
${voffset -9}
${alignc}${font Montserrat Light:size=10}${time %A %d %B %Y}${font}${alignc}
${color6}${hr 2}${color} 
${voffset -6}
${font Montserrat Light:size=8}${color1}SYSTEM:${color}${font} ${alignr}$sysname $nodename 
${font Montserrat Light:size=8}${color1}UPTIME:${color}${font} ${alignr}$uptime_short
${font Montserrat Light:size=8}${color1}KERNEL:${color}${font} ${alignr}$kernel ${voffset 5}
${font :size=11}${color}CPUs ${color}${hr 2}${color}
${voffset -15}
${font Montserrat Light:size=10}${color1}CPU:${color}${font} ${alignr}${color}AMD Ryzen 5 @ 3.60GHz${color}${font}
${voffset -15}
${font Montserrat Light:size=9}${color1}01 - 04:${color}${font} ${goto 100}${cpu cpu1}% ${goto 150}${cpu cpu2 }% ${goto 200}${cpu cpu3 }% ${goto 250}${cpu cpu4 }%
${font Montserrat Light:size=9}${color1}05 - 08:${color}${font} ${goto 100}${cpu cpu5}% ${goto 150}${cpu cpu6 }% ${goto 200}${cpu cpu7 }% ${goto 250}${cpu cpu8 }%
${font Montserrat Light:size=9}${color1}09 - 12:${color}${font} ${goto 100}${cpu cpu9}% ${goto 150}${cpu cpu10}% ${goto 200}${cpu cpu11}% ${goto 250}${cpu cpu12}%
${voffset -8}
${font Montserrat Light:size=9}${color1}CPU TEMP:${color}${font} ${font :size=10}${alignr}${hwmon 1 temp 1}°${color4}C${color}${voffset 5}
${font :size=11}${color}Network ${color}${hr 2}${color}
${voffset -15}
${font Montserrat Light:size=10}${color1}IP ADDRESS:${color}${font} ${alignr}${addr enp6s0}
${if_match "${addr enp6s0}"!="No Address"}${voffset 5}${font Montserrat Light:size=10}${font}Download ${alignr}${downspeedf enp6s0}k/s (${totaldown enp6s0})
${color3}${downspeedgraph enp6s0 50,280 ADFF2F 32CD32 -t}
${font Montserrat Light:size=10}${font}Upload ${alignr}${upspeedf enp6s0}k/s (${totalup enp6s0})
${color3}${upspeedgraph enp6s0 50,280 FF0000 8B0000 -t}
${endif}${voffset 5}${font :size=11}${color}HDD ${color}${hr 2}${color}
${color1}Used: ${color3}${fs_used /}${color1}${goto 200}Free:${goto 250}${color3}${fs_free /}
${color2}${fs_bar /}
${font Montserrat Light:size=8}${color1}DISK I/O:${color}${font} ${alignr}$diskio
${voffset 2}${font Montserrat Light:size=8}${color1}READ: ${color}${font} ${goto 80}${color4}${diskiograph_read  15,210 ADFF2F 32CD32 750}${color}
${voffset 2}${font Montserrat Light:size=8}${color1}WRITE:${color}${font} ${goto 80}${color4}${diskiograph_write 15,210 FF0000 8B0000 750}${color}
${font :size=11}${color}Memory ${color}${hr 2}${color}${voffset 2}
$font${color DimGray}RAM $alignc $mem / $memmax $alignr ${color 188f18}$memperc%
${color 188f18}$membar${voffset 1}
${font :size=11}${color}GPU ${color}${hr 2}${color}${voffset 2}
${font Montserrat Light:size=9}${color1}GPU Freq.: $alignr ${color}${font}${nvidia gpufreq} Mhz${voffset 3}
${font Montserrat Light:size=9}${color1}Memory Freq.: $alignr ${color}${font}${nvidia memfreq} Mhz${voffset 3}
${font Montserrat Light:size=9}${color1}Temperature: $alignr ${color}${font}${nvidia temp}°C ${voffset 3}
${voffset 3}${font :size=11}${color}Processes ${color}${hr 2}${color}
${voffset -15}
${font :size=10}${font Montserrat Light:size=8}${color1}TOTAL:${color}${font} ${alignr}${processes}
${voffset -10} 
${font Montserrat Light:size=9}${color1}APP NAME: ${goto 160}MEMORY: ${goto 245}CPU: ${color}${font}
${voffset -16}
${font Montserrat Light:size=9}${color1}${top_mem name 1} ${color}${font} ${goto 160}${top mem 1} % ${goto 235}${top cpu 1} %
${font Montserrat Light:size=9}${color1}${top_mem name 2} ${color}${font} ${goto 160}${top mem 2} % ${goto 235}${top cpu 2} %
${font Montserrat Light:size=9}${color1}${top_mem name 3} ${color}${font} ${goto 160}${top mem 3} % ${goto 235}${top cpu 3} %
${font Montserrat Light:size=9}${color1}${top_mem name 4} ${color}${font} ${goto 160}${top mem 4} % ${goto 235}${top cpu 4} %
${font Montserrat Light:size=9}${color1}${top_mem name 5} ${color}${font} ${goto 160}${top mem 5} % ${goto 235}${top cpu 5} %
${font Montserrat Light:size=9}${color1}${top_mem name 6} ${color}${font} ${goto 160}${top mem 6} % ${goto 235}${top cpu 6} %
${font Montserrat Light:size=9}${color1}${top_mem name 7} ${color}${font} ${goto 160}${top mem 7} % ${goto 235}${top cpu 7} %
${font Montserrat Light:size=9}${color1}${top_mem name 8} ${color}${font} ${goto 160}${top mem 8} % ${goto 235}${top cpu 8} %
${font Montserrat Light:size=9}${color1}${top_mem name 9} ${color}${font} ${goto 160}${top mem 9} % ${goto 235}${top cpu 9} %
${font Montserrat Light:size=9}${color1}${top_mem name 10} ${color}${font} ${goto 160}${top mem 10} % ${goto 235}${top cpu 10} %

${font Montserrat Light:size=10}${color1}${alignr}by: Mo Abdrabou${color}${font}


]];
