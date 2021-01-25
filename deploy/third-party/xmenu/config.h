static struct Config config = {
	/* font, separate different fonts with comma */
	.font = "Operator Mono Lig Book:pixelsize=10:antialias=true:autohint=true",

	/* colors */
	.background_color = "#151a1e",
	.foreground_color = "#eaeaea",
	.selbackground_color = "#B8CC52",
	.selforeground_color = "#151a1e",
	.separator_color = "#36A3D9",
	.border_color = "#B8CC52",

	/* sizes in pixels */
	.width_pixels = 130,        /* minimum width of a menu */
	.height_pixels = 25,        /* height of a single menu item */
	.border_pixels = 1,         /* menu border */
	.separator_pixels = 2,      /* space around separator */
	.gap_pixels = 0,            /* gap between menus */

	/*
	 * The variables below cannot be set by X resources.
	 * Their values must be less than .height_pixels.
	 */

	/* geometry of the right-pointing isoceles triangle for submenus */
	.triangle_width = 3,
	.triangle_height = 7,

	/* the icon size is equal to .height_pixels - .iconpadding * 2 */
	.iconpadding = 3,

	/* area around the icon, the triangle and the separator */
	.horzpadding = 8,
};

/*
 * KEYBINDINGS
 *
 * Look at your /usr/include/X11/keysymdef.h  (or the equivalent file
 * in your system) for a list of key symbol constants, and change the
 * macros below accordingly.  All key symbol constants begin with the
 * prefix XK_.
 *
 * For example, to use vim-like key bindings, set KEYSYMLEFT to XK_h,
 * KEYSYMDOWN to XK_j, KEYSYMUP to XK_k, etc.
 *
 * Note that the regular keys like ArrowUp, ArrowDown, Tab, Home, etc
 * will ALWAYS work, so you do not need to set them.
 *
 * If you do not want to set a key binding, keep it with the value of
 * XK_VoidSymbol
 */
#define KSYMFIRST   XK_p       /* select first item */
#define KSYMLAST    XK_n       /* select last item */
#define KSYMUP      XK_k       /* select previous item */
#define KSYMDOWN    XK_j       /* select next item */
#define KSYMLEFT    XK_h       /* close current menu */
#define KSYMRIGHT   XK_l       /* enter selected item */
