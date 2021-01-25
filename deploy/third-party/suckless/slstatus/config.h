const unsigned int interval = 1000;
static const char unknown_str[] = "n/a";

#define MAXLEN 2048

/*
 * function            description                     argument (example)
 * 
 * battery_perc        battery percentage              battery name (BAT0)
 * battery_state       battery charging state          battery name (BAT0)
 * battery_remaining   battery remaining HH:MM         battery name (BAT0)
 * cpu_perc            cpu usage in percent            NULL
 * cpu_freq            cpu frequency in MHz            NULL
 * datetime            date and time                   format string (%F %T)
 * disk_free           free disk space in GB           mountpoint path (/)
 * disk_total          total disk space in GB          mountpoint path (/")
 * netspeed_rx         receive network speed           interface name (wlan0)
 * netspeed_tx         transfer network speed          interface name (wlan0)
 * ram_perc            memory usage in percent         NULL
 * run_command         custom shell command            command (echo foo)
 * separator           string to echo                  NULL
 * swap_perc           swap usage in percent           NULL
 * temp                temperature in degree celsius   sensor file
 * uptime              system uptime                   NULL
 * vol_perc            OSS/ALSA volume in percent      mixer file (/dev/mixer)

 * disk_perc           disk usage in percent           mountpoint path (/)
 * disk_used           used disk space in GB           mountpoint path (/)
 * entropy             available entropy               NULL
 * gid                 GID of current user             NULL
 * hostname            hostname                        NULL
 * ipv4                IPv4 address                    interface name (eth0)
 * ipv6                IPv6 address                    interface name (eth0)
 * kernel_release      `uname -r`                      NULL
 * keyboard_indicators caps/num lock indicators        format string (c?n?) [see keyboard_indicators.c]
 * keymap              layout of current keymap        NULL
 * load_avg            load average                    NULL
 * num_files           number of files in a directory  path
 * ram_used            used memory in GB               NULL
 * ram_total           total memory size in GB         NULL
 * ram_free            free memory in GB               NULL
 * swap_free           free swap in GB                 NULL
 * swap_total          total swap size in GB           NULL
 * swap_used           used swap in GB                 NULL
 * uid                 UID of current user             NULL
 * username            username of current user        NULL
 * wifi_perc           WiFi signal in percent          interface name (wlan0)
 * wifi_essid          WiFi ESSID                      interface name (wlan0)
 */

static const struct arg args[] = {
	/* function          format        argument */
	{ netspeed_rx,       " %sﯲ ",     "wlan0" },
	{ netspeed_tx,       "%sﯴ B/s",    "wlan0" },
	{ separator,         "|",          NULL },
	{ disk_free,         " %sB ",     "/" },
	{ ram_perc,          " %s%%",     NULL },
	{ swap_perc,         "(%s%%) ",    NULL },
	{ cpu_perc,          " %s%%",     NULL },
	{ cpu_freq,          "(%sHz) ",    NULL },
	{ temp,              " :%s ",     "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp1_input" },
	{ temp,              "%s ",        "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp2_input" },
	{ temp,              "%s ",        "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp3_input" },
	{ temp,              "%s ",        "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp4_input" },
	{ temp,              "%s°C ",      "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp5_input" },
	{ separator,         "|",          NULL },
	{ uptime,            "祥 %s",      NULL },
	{ separator,         "|",          NULL },
	{ run_command,       "%s",         "sls-volume" },
	{ separator,         "|",          NULL },
	{ battery_perc,      " %s%%",     "BAT0" },
	{ battery_state,     " (%s",       "BAT0" },
	{ battery_remaining, "%s)",        "BAT0" },
	{ separator,         "|",          NULL },
	{ datetime,          "%s",         "%T %a%e %b" },
};

// vim:ft=c:nospell
