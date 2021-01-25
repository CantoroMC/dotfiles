// Appearance
static const unsigned int borderpx       = 3;            /* border pixel of windows */
static const unsigned int snap           = 24;           /* snap pixel */
static const Gap default_gap             = {.isgap = 1, .realgap = 0, .gappx = 0};
static const int showbar                 = 1;            /* 0 means no bar */
static const int topbar                  = 1;            /* 0 means bottom bar */
static const char buttonbar[]            = "Menu";
static const unsigned int systraypinning = 1;            /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 2;            /* systray spacing */
// 1: if pinning fails, display systray on the first monitor,
//    False: display systray on the last monitor
static const int systraypinningfailfirst = 1;
static const int showsystray             = 1;            /* 0 means no systray */
// Display modes of the tab bar:
//   never shown, always shown, shown only in monocle mode in presence of several windows.
// Modes after showtab_nmodes are disabled
enum showtab_modes { showtab_never, showtab_auto, showtab_nmodes, showtab_always};
static const int showtab                 = showtab_auto; /* Default tab bar show mode */
static const Bool toptab                 = True;        /* False means bottom tab bar */
static const char *fonts[]               = {
	"Operator Mono Lig Book:size=10.0",
	"FiraCode Nerd Font Book:size=10.0",
};
static const char norm_bg[]              = "#151a1e";
static const char norm_border[]          = "#151a1e";
static const char norm_fg[]              = "#b8cc52";
static const char self_fg[]              = "#eaeaea";
static const char self_bg[]              = "#151a1e";
static const char self_border[]          = "#b8cc52";
static const char *colors[][3]           = {
	[SchemeNorm] = { norm_fg, norm_bg, norm_border },
	[SchemeSel]  = { self_fg, self_bg, self_border },
};



// Tagging and Rules
static const char *tags[] = { "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι" };
// default layout per tags
// The first element is for all-tag view, following i-th element corresponds to tags[i].
// Layout is referred using the layouts array index.
static int def_layouts[1 + LENGTH(tags)]  = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static const Rule rules[] = {
	/* class                          instance     title             tags mask  iscentered isfloating  monitor  scratch key */
	{ "Arandr",                       NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Avahi-discover",               NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Baobab",                       NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Blueberry.py",                 NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Bssh",                         NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Bvnc",                         NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "CMakeSetup",                   NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Exo-helper-2",                 NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "feh",                          NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Gimp",                         NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Gnome-disks",                  NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Gpick",                        NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Hardinfo",                     NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "imagewriter",                  NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Lxappearance",                 NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "MPlayer",                      NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Nitrogen",                     NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "ParaView",                     NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Parcellite",                   NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Pavucontrol",                  NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "qv4l2",                        NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "qvidcap",                      NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "System-config-printer.py",     NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Sxiv",                         NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Xarchiver",                    NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Xboard",                       NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Xfce4-about",                  NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Xmessage",                     NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Yad",                          NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ "Yad-icon-browser",             NULL,       NULL,               0,       1,          1,          -1,      0 },
	{ NULL,                           NULL,       "Event Tester",     0,       1,          1,          -1,      0 },
	{ NULL,                           NULL,       "lstopo",           0,       1,          1,          -1,      0 },
	{ NULL,                           NULL,       "weatherreport",    0,       1,          1,          -1,      0 },
	{ NULL,                           "pop-up",   NULL,               0,       1,          1,          -1,      0 },
	{ "Display",                      NULL,       "ImageMagick: ",    0,       1,          1,          -1,      0 },
	{ "MATLAB R2019b - academic use", NULL,       "Help",             0,       1,          1,          -1,      0 },
	{ "Transmission-gtk",             NULL,       NULL,               1 << 8,  1,          1,          -1,      0 },
	{ "mpv",                          NULL,       NULL,               1 << 4,  0,          0,          -1,      0 },
	{ NULL,                           NULL,       "MATLAB",           1 << 1,  0,          0,          -1,      0 },
	{ NULL,                           NULL,       "yakuake",          0,       1,          1,          -1,      'y' },
	{ NULL,                           NULL,       "cmus",             0,       1,          1,          -1,      'm' },
	{ NULL,                           NULL,       "orgenda",          0,       1,          1,          -1,      'o' },
	/* class                          instance     title             tags mask  iscentered isfloating  monitor  scratch key */
};



// Layouts
static const int focusonwheel    = 0;
static const float mfact         = 0.5;  /* factor of master area size [0.05..0.95] */
static const float smfact        = 0.00; /* factor of tiled clients [0.00..0.95] */
static const int nmaster         = 1;    /* number of clients in master area */
static const unsigned int minwsz = 20;   /* Minimal heigt of a client for smfact */
static const int resizehints     = 0;    /* 1 means respect size hints in tiled resizals */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },
	{ "[D]",      deck },
	{ "TTT",      bstack },
	{ ">>=",      NULL },
	{ "|+|",      tatami },
	{ "|||",      tcl },
	{ "[M]",      monocle },
	{ NULL,       NULL },
};



// Key Definitions
static const char *layoutmenu_cmd = "xmenu_dwmlayout";
static char dmenumon[2]           = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *termcmd[]      = { "st", NULL };
static const char *browsercmd[]   = { "vivaldi-stable", NULL };
static const char *dmenucmd[]     = { "dmenu_run", "-m", dmenumon, NULL };
static const char *roficmd[]      = { "rofi", "-modi", "drun,run,combi", "-show", "combi", NULL};
static const char *xmenucmd[]     = { "xmenu-apps", NULL};
static const char *xmenuexitcmd[] = { "xmenu-shutdown", NULL};


#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }
#define TERMINAL "st"

/*First arg only serves to match against key in rules*/
static const char *yakuakecmd[] = {"y", TERMINAL, "-t", "yakuake", NULL};
static const char *cmuscmd[] = {"m", TERMINAL, "-t", "cmus", "-e", "cmus", NULL};
static const char *orgendacmd[] = {"o", "emacs", "--name=orgenda", "~/Documents/organization/Notes.org", NULL};

static Key keys[] = {
	/* modifier                     key        function        argument */

	/*                              Left Side                           */

	{ MODKEY,                       XK_q,            resetlayout,    {0} },
	{ MODKEY|ShiftMask,             XK_q,            killclient,     {0} },
	// Monitors: shift and move to previous/next
	{ MODKEY,                       XK_w,            focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_e,            focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_w,            tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_e,            tagmon,         {.i = +1 } },
	// Go to previous tag/layout
	{ MODKEY,                       XK_r,            view,           {0} },
	{ MODKEY|ShiftMask,             XK_r,            setlayout,      {0} },
	{ MODKEY,                       XK_t,            togglefloating, {0} },
	{ MODKEY|ShiftMask,             XK_t,            tabmode,        {-1} },
	// Gaps or Spacing
	{ MODKEY,                       XK_a,            spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_s,            setgaps,        {.i = +3 } },
	{ MODKEY|ShiftMask,             XK_s,            setgaps,        {.i = -3 } },
	{ MODKEY|ControlMask,           XK_s,            setgaps,        {.i = GAP_RESET } },
	{ MODKEY|ShiftMask|ControlMask, XK_s,            setgaps,        {.i = GAP_TOGGLE} },
	// Emacs and Nautilus
	{ MODKEY,                       XK_d,            spawn,          SHCMD("emacs") },
	{ MODKEY|ShiftMask,             XK_d,            spawn,          SHCMD("nautilus") },

	{ MODKEY,                       XK_f,            spawn,          {.v = browsercmd } },
	{ MODKEY|ShiftMask,             XK_f,            spawn,          SHCMD("surf-open") },
	{ MODKEY,                       XK_b,            togglebar,      {0} },

	/*                              Right Side                                           */

	// Music Players
	{ MODKEY,                       XK_p,            spawn,          SHCMD(TERMINAL " -e ncmpcpp")},
	{ MODKEY|ShiftMask,             XK_p,            spawn,          SHCMD(TERMINAL " -e cmus")},
	// Focus previous/next client
	{ MODKEY,                       XK_k,            focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_j,            focusstack,     {.i = +1 } },
	// Move the focused client forward/backward in the stack
	{ MODKEY|ShiftMask,             XK_k,            movestack,      {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,            movestack,      {.i = +1 } },
	// Resize slave clients
	{ MODKEY|ShiftMask,             XK_l,            setsmfact,      {.f = -0.05} },
	{ MODKEY|ShiftMask,             XK_h,            setsmfact,      {.f = +0.05} },
	// Resize Master fraction
	{ MODKEY,                       XK_h,            setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,            setmfact,       {.f = +0.05} },
	// focus the master client
	{ MODKEY,                       XK_m,            focusmaster,    {0} },
	// toggle between master and stack
	{ MODKEY|ShiftMask,             XK_m,            zoom,           {0} },
	// Menu launchers
	{ MODKEY,                       XK_u,            spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_u,            spawn,          {.v = roficmd } },
	{ MODKEY|ControlMask,           XK_u,            spawn,          {.v = xmenucmd } },

	// Scratchpads
	{ MODKEY|ShiftMask|ControlMask, XK_y,            togglescratch,  {.v = yakuakecmd } },
	{ MODKEY|ShiftMask|ControlMask, XK_m,            togglescratch,  {.v = cmuscmd } },
	{ MODKEY|ShiftMask|ControlMask, XK_o,            togglescratch,  {.v = orgendacmd } },

	/*                              Surrounding Keys                                     */

	// Master/Stack Vertical Layout
	{ MODKEY,                       XK_Tab,          cyclelayout,    {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_Tab,          cyclelayout,    {.i = -1 } },
	// Floating Layout
	{ MODKEY,                       XK_space,        setlayout,      {.v = &layouts[3]} },
	// Monocle Layout
	{ MODKEY|ShiftMask,             XK_space,        setlayout,      {.v = &layouts[6]} },
	// Decrease/Increase Number of masters
	{ MODKEY,                       XK_period,       incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_comma,        incnmaster,     {.i = +1 } },

	{ MODKEY,                       XK_bracketleft,  shiftview,      { .i = -1 } },
	{ MODKEY,                       XK_bracketright, shiftview,      { .i = +1 } },
	{ MODKEY|ShiftMask,             XK_bracketleft,  shifttag,       { .i = -1 } },
	{ MODKEY|ShiftMask,             XK_bracketright, shifttag,       { .i = +1 } },

	{ MODKEY,                       XK_Return,       spawn,          {.v = termcmd } },
	{ MODKEY|ShiftMask,             XK_Return,       spawn,          SHCMD("kitty") },
	{ MODKEY|ShiftMask,             XK_BackSpace,    spawn,          SHCMD("slock") },
	{ MODKEY,                       XK_Delete,       spawn,          {.v = xmenuexitcmd } },
	{ MODKEY|ShiftMask,             XK_Delete,       quit,           {0} },

	{ MODKEY,                       XK_Print,       spawn,          SHCMD("scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshoot.png; notify-send -i photo 'Taken fullscreen screenshot'") },
	{ MODKEY|ShiftMask,             XK_Print,       quit,           SHCMD("import ~/Pictures/Screenshots/$(date +%Y-%m-%d-%T)screenshoot.png; notify-send -i photo 'Saved to ~/Pictures/Screenshots'") },

	/*                              Arrows                                             */

	{ MODKEY,                       XK_Down,         moveresize,     {.v = "0x 25y 0w 0h"  } },
	{ MODKEY,                       XK_Up,           moveresize,     {.v = "0x -25y 0w 0h" } },
	{ MODKEY,                       XK_Right,        moveresize,     {.v = "25x 0y 0w 0h"  } },
	{ MODKEY,                       XK_Left,         moveresize,     {.v = "-25x 0y 0w 0h" } },
	{ MODKEY|ShiftMask,             XK_Down,         moveresize,     {.v = "0x 0y 0w 25h"  } },
	{ MODKEY|ShiftMask,             XK_Up,           moveresize,     {.v = "0x 0y 0w -25h" } },
	{ MODKEY|ShiftMask,             XK_Right,        moveresize,     {.v = "0x 0y 25w 0h"  } },
	{ MODKEY|ShiftMask,             XK_Left,         moveresize,     {.v = "0x 0y -25w 0h" } },
	{ MODKEY|ControlMask,           XK_Down,         spawn,          SHCMD("mpc pause")},
	{ MODKEY|ControlMask,           XK_Up,           spawn,          SHCMD("mpc play")},
	{ MODKEY|ControlMask,           XK_Right,        spawn,          SHCMD("mpc next")},
	{ MODKEY|ControlMask,           XK_Left,         spawn,          SHCMD("mpc prev")},

	/*                              Numbers                                              */

	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)

	/*                              Fn and Extra Keys                                    */
	{ 0, XF86XK_AudioMute,         spawn,  SHCMD("pactl set-sink-mute @DEFAULT_SINK@ toggle") },
	{ 0, XF86XK_AudioLowerVolume,  spawn,  SHCMD("pactl set-sink-volume @DEFAULT_SINK@ -5%") },
	{ 0, XF86XK_AudioRaiseVolume,  spawn,  SHCMD("pactl set-sink-volume @DEFAULT_SINK@ +5%") },
	{ 0, XF86XK_MonBrightnessDown, spawn,  SHCMD("xbacklight -dec 5") },
	{ 0, XF86XK_MonBrightnessUp,   spawn,  SHCMD("xbacklight -inc 5") },
	{ 0, XF86XK_Display,           spawn,  SHCMD("monitor_handler") },
	{ 0, XF86XK_Search,            spawn,  SHCMD(TERMINAL " -e ranger") },
	{ 0, XF86XK_Explorer,          spawn,  SHCMD("nautilus") },
	{ 0, XF86XK_Calculator,        spawn,  SHCMD(TERMINAL " -e ghci") },
};



// Mouse Button Definitions
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkButton,            0,              Button1,        spawn,          {.v = xmenucmd } },
	{ ClkButton,            0,              Button3,        spawn,          {.v = xmenuexitcmd } },
	{ ClkButton,            0,              Button2,        spawn,          SHCMD("weather") },
	{ ClkButton,            0,              Button4,        spawn,          SHCMD("xbacklight -inc 5") },
	{ ClkButton,            0,              Button5,        spawn,          SHCMD("xbacklight -dec 5") },

	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkTagBar,            0,              Button4,        shiftview,      {.i = +1} },
	{ ClkTagBar,            0,              Button5,        shiftview,      {.i = -1} },

	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button2,        setlayout,      {.v = &layouts[6]} },
	{ ClkLtSymbol,          0,              Button3,        layoutmenu,     {0} },
	{ ClkLtSymbol,          0,              Button4,        cyclelayout,    {.i = +1 } },
	{ ClkLtSymbol,          0,              Button5,        cyclelayout,    {.i = -1 } },

	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkWinTitle,          0,              Button4,        movestack,      {.i = +1 } },
	{ ClkWinTitle,          0,              Button5,        movestack,      {.i = -1 } },

	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkStatusText,        0,              Button3,        spawn,          SHCMD(TERMINAL " -e htop") },
	{ ClkStatusText,        0,              Button4,        spawn,          SHCMD("pactl set-sink-volume @DEFAULT_SINK@ +5%") },
	{ ClkStatusText,        0,              Button5,        spawn,          SHCMD("pactl set-sink-volume @DEFAULT_SINK@ -5%") },

	{ ClkRootWin,           0,              Button2,        togglebar,      {0} },

	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },

	{ ClkTabBar,            0,              Button1,        focuswin,       {0} },
	{ ClkTabBar,            0,              Button2,        togglefloating, {0} },
};

// vim:ft=c:nospell:tw=0
