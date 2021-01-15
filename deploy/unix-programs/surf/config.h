/* modifier 0 means no modifier */
static int surfuseragent    = 1;  /* Append Surf version to default WebKit user agent */
static char *fulluseragent  = ""; /* Or override the whole user agent string */
static char *scriptfile     = "~/.config/surf/script.js";
static char *styledir       = "~/.config/surf/styles/";
static char *certdir        = "~/.local/share/surf/certificates/";
static char *cachedir       = "~/.cache/surf/";
static char *cookiefile     = "~/.local/share/surf/cookies.txt";
static char *dldir          = "~/Downloads/";
static char *dlstatus       = "~/.local/share/surf/dlstatus/";
static char **plugindirs    = (char*[]){
	"~/.config/surf/plugins/",
	LIBPREFIX "/mozilla/plugins/",
	NULL
};

/* Webkit default features */
/* Highest priority value will be used.
 * Default parameters are priority 0
 * Per-uri parameters are priority 1
 * Command parameters are priority 2
 */
static Parameter defconfig[ParameterLast] = {
	/* parameter                    Arg value       priority */
	[AcceleratedCanvas]   =       { { .i = 1 },     },
	[AccessMicrophone]    =       { { .i = 0 },     },
	[AccessWebcam]        =       { { .i = 0 },     },
	[Certificate]         =       { { .i = 0 },     },
	[CaretBrowsing]       =       { { .i = 0 },     },
	[CookiePolicies]      =       { { .v = "@Aa" }, },
	[DefaultCharset]      =       { { .v = "UTF-8" }, },
	[DiskCache]           =       { { .i = 1 },     },
	[DNSPrefetch]         =       { { .i = 0 },     },
	[Ephemeral]           =       { { .i = 0 },     },
	[FileURLsCrossAccess] =       { { .i = 0 },     },
	[FontSize]            =       { { .i = 12 },    },
	[FrameFlattening]     =       { { .i = 0 },     },
	[Geolocation]         =       { { .i = 0 },     },
	[HideBackground]      =       { { .i = 0 },     },
	[Inspector]           =       { { .i = 0 },     },
	[Java]                =       { { .i = 1 },     },
	[JavaScript]          =       { { .i = 1 },     },
	[KioskMode]           =       { { .i = 0 },     },
	[LoadImages]          =       { { .i = 1 },     },
	[MediaManualPlay]     =       { { .i = 1 },     },
	[Plugins]             =       { { .i = 1 },     },
	[PreferredLanguages]  =       { { .v = (char *[]){ NULL } }, },
	[RunInFullscreen]     =       { { .i = 0 },     },
	[ScrollBars]          =       { { .i = 1 },     },
	[ShowIndicators]      =       { { .i = 1 },     },
	[SiteQuirks]          =       { { .i = 1 },     },
	[SmoothScrolling]     =       { { .i = 0 },     },
	[SpellChecking]       =       { { .i = 0 },     },
	[SpellLanguages]      =       { { .v = ((char *[]){ "en_US", NULL }) }, },
	[StrictTLS]           =       { { .i = 1 },     },
	[Style]               =       { { .i = 1 },     },
	[WebGL]               =       { { .i = 0 },     },
	[ZoomLevel]           =       { { .f = 1.0 },   },
	[ClipboardNotPrimary] =       { { .i = 1 }      },
};

static UriParameters uriparams[] = {
	{ "(://|\\.)suckless\\.org(/|$)", {
	  [JavaScript] = { { .i = 0 }, 1 },
	  [Plugins]    = { { .i = 0 }, 1 },
	}, },
};

/* default window size: width, height */
static int winsize[] = { 800, 600 };

static WebKitFindOptions findopts = WEBKIT_FIND_OPTIONS_CASE_INSENSITIVE |
                                    WEBKIT_FIND_OPTIONS_WRAP_AROUND;

#define PROMPT_GO   "Go:"
#define PROMPT_FIND "Find:"
#define HOMEPAGE "https://duckduckgo.com/"

/* SETPROP(readprop, setprop, prompt)*/
#define SETPROP(r, s, p) { \
	.v = (const char *[]){ "/bin/sh", "-c", \
		"prop=\"$(printf '%b' \"$(xprop -id $1 $2 " \
		"| sed \"s/^$2(STRING) = //;s/^\\\"\\(.*\\)\\\"$/\\1/\" && cat ~/.local/share/surf/bookmarks)\" " \
		"| dmenu -l 5 -p \"$4\" -w $1)\" && " \
		"xprop -id $1 -f $3 8s -set $3 \"$prop\"", \
		"surf-setprop", winid, r, s, p, NULL \
	} \
}

#define DLSTATUS { \
	.v = (const char *[]){ "st", "-e", "/bin/sh", "-c", \
			"while true; do cat $1/* 2>/dev/null || echo \"no hay descargas\";" \
			"A=; read A; " \
			"if [ $A = \"clean\" ]; then rm $1/*; fi; clear; done", \
			"surf-dlstatus", dlstatus, NULL \
	} \
}

/* PLUMB(URI) */
/* This called when some URI which does not begin with "about:",
 * "http://" or "https://" should be opened.
 */
#define PLUMB(u) {\
        .v = (const char *[]){ "/bin/sh", "-c", \
             "xdg-open \"$0\"", u, NULL \
        } \
}

/* VIDEOPLAY(URI) */
#define VIDEOPLAY(u) {\
        .v = (const char *[]){ "/bin/sh", "-c", \
             "mpv --really-quiet \"$0\"", u, NULL \
        } \
}

/* BM_ADD(readprop) */
#define BM_ADD(r) {\
	.v = (const char *[]){ "/bin/sh", "-c", \
		"(echo $(xprop -id $0 $1) | cut -d '\"' -f2 " \
		"| sed 's/.*https*:\\/\\/\\(www\\.\\)\\?//' && cat ~/.local/share/surf/bookmarks) " \
		"| awk '!seen[$0]++' > ~/.local/share/surf/bookmarks.tmp && " \
		"mv ~/.local/share/surf/bookmarks.tmp ~/.local/share/surf/bookmarks", \
		winid, r, NULL \
	} \
}

/* styles */
/*
 * The iteration will stop at the first match, beginning at the beginning of
 * the list.
 */
static SiteSpecific styles[] = {
	/* regexp                     file in $styledir */
	{ ".*.wikipedia.org.*",    "inverted.css" },
	{ ".*",                    "default.css" },
};

/* certificates */
/*
 * Provide custom certificate for urls
 */
static SiteSpecific certs[] = {
	/* regexp               file in $certdir */
	{ "://suckless\\.org/", "suckless.org.crt" },
};

/* Search Engines */
static SearchEngine searchengines[] = {
	{ "a",   "https://wiki.archlinux.org/index.php/Special:Search?search=%s&go=Go" },
	{ "d",   "http://www.duckduckgo.com/search?q=%s" },
	{ "g",   "http://www.google.com/search?q=%s" },
	{ "leo", "http://dict.leo.org/ende?search=%s" },
	{ "s",   "https://startpage.com/do/search?q=%s" },
	{ "wd",  "https://en.wiktionary.org/w/index.php?search=%s&go=Go" },
	{ "w",   "https://en.wikipedia.org/wiki/index.php/Special:Search?search=%s&go=Go" },

};

#define MODKEY GDK_CONTROL_MASK

/* hotkeys */
/*
 * If you use anything else but MODKEY and GDK_SHIFT_MASK, don't forget to
 * edit the CLEANMASK() macro.
 */
static Key keys[] = {
	/* modifier              keyval          function    arg */
	// Dmenu Bindings
	{ 0,                     GDK_KEY_g,      spawn,      SETPROP("_SURF_URI", "_SURF_GO", PROMPT_GO) },
	{ 0,                     GDK_KEY_m,      spawn,      BM_ADD("_SURF_URI") },
	{ 0,                     GDK_KEY_slash,  spawn,      SETPROP("_SURF_FIND", "_SURF_FIND", PROMPT_FIND) },

	/* download-console */
	{ MODKEY,                GDK_KEY_d,      spawndls,   { 0 } },

	// Stop and Reload and Insert mode
	{ 0,                     GDK_KEY_c,      stop,       { 0 } },
	{ 0,                     GDK_KEY_Escape, insert,     { .i = 0 } },
	{ 0,                     GDK_KEY_i,      insert,     { .i = 1 } },
	{ MODKEY,                GDK_KEY_r,      reload,     { .i = 1 } },
	{ 0,                     GDK_KEY_r,      reload,     { .i = 0 } },

	// History Navigation
	{ 0,                     GDK_KEY_i,      navigate,   { .i = +1 } },
	{ 0,                     GDK_KEY_o,      navigate,   { .i = -1 } },

	/* vertical and horizontal scrolling, in viewport percentage */
	{ 0,                     GDK_KEY_l,      scrollh,    { .i = +10 } },
	{ 0,                     GDK_KEY_h,      scrollh,    { .i = -10 } },
	{ 0,                     GDK_KEY_d,      scrollv,    { .i = +10 } },
	{ 0,                     GDK_KEY_u,      scrollv,    { .i = -10 } },
	{ 0,                     GDK_KEY_f,      scrollv,    { .i = +50 } },
	{ 0,                     GDK_KEY_b,      scrollv,    { .i = -50 } },
	{ 0,                     GDK_KEY_j,      scrollv,    { .i = +1 } },
	{ 0,                     GDK_KEY_k,      scrollv,    { .i = -1 } },

	// Zooms
	{ 0|GDK_SHIFT_MASK,      GDK_KEY_j,      zoom,       { .i = -1 } },
	{ 0|GDK_SHIFT_MASK,      GDK_KEY_k,      zoom,       { .i = +1 } },
	{ 0|GDK_SHIFT_MASK,      GDK_KEY_q,      zoom,       { .i = 0  } },
	{ 0,                     GDK_KEY_minus,  zoom,       { .i = -1 } },
	{ 0,                     GDK_KEY_plus,   zoom,       { .i = +1 } },

	// Clipboard
	{ 0,                     GDK_KEY_p,      clipboard,  { .i = 1 } },
	{ 0,                     GDK_KEY_y,      clipboard,  { .i = 0 } },

	{ 0,                     GDK_KEY_n,      find,       { .i = +1 } },
	{ 0|GDK_SHIFT_MASK,      GDK_KEY_n,      find,       { .i = -1 } },

	{ MODKEY,                GDK_KEY_p,      print,      { 0 } },
	{ MODKEY,                GDK_KEY_t,      showcert,   { 0 } },

	// Toggle Options
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_a,      togglecookiepolicy, { 0 } },
	{ 0,                     GDK_KEY_F11,    togglefullscreen, { 0 } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_o,      toggleinspector, { 0 } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_c,      toggle,     { .i = CaretBrowsing } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_f,      toggle,     { .i = FrameFlattening } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_g,      toggle,     { .i = Geolocation } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_s,      toggle,     { .i = JavaScript } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_i,      toggle,     { .i = LoadImages } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_v,      toggle,     { .i = Plugins } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_b,      toggle,     { .i = ScrollBars } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_t,      toggle,     { .i = StrictTLS } },
	{ MODKEY|GDK_SHIFT_MASK, GDK_KEY_m,      toggle,     { .i = Style } },
};

/* button definitions */
/* target can be OnDoc, OnLink, OnImg, OnMedia, OnEdit, OnBar, OnSel, OnAny */
static Button buttons[] = {
	/* target       event mask      button  function        argument        stop event */
	{ OnLink,       0,              2,      clicknewwindow,    { .i = 0 },     1 },
	{ OnLink,       MODKEY,         2,      clicknewwindow,    { .i = 1 },     1 },
	{ OnLink,       MODKEY,         1,      clicknewwindow,    { .i = 1 },     1 },
	{ OnAny,        0,              8,      clicknavigate,     { .i = -1 },    1 },
	{ OnAny,        0,              9,      clicknavigate,     { .i = +1 },    1 },
	{ OnMedia,      MODKEY,         1,      clickexternplayer, { 0 },          1 },
};
