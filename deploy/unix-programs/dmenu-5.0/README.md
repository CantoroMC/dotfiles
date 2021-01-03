dmenu - dynamic menu
====================

dmenu is an efficient dynamic menu for X.

Requirements
------------
In order to build dmenu you need the Xlib header files.
Installation
------------
Edit config.mk to match your local setup (dmenu is installed into
the /usr/local namespace by default).

Afterwards enter the following command to build and install dmenu
(if necessary as root):

    make clean install

Running dmenu
-------------
See the man page for details.

Patches
-------
- **case-insensitive**:  
    This patch changes case-insensitive item matching to default behaviour.
    Adds an -s option to enable case-sensitive matching.
    I modify it to -i to replace the original case insensitivity and avoid
    any conflict with packages that adopt the -i flag.
    I rather use it with case sensitivity that modify installed packages to 
    make dmenu work.
- **center**:  
    This patch add a flag (-c) which centers dmenu in the middle of the screen.
- **fuzzymatch**:  
    This patch adds support for fuzzy-matching to dmenu, allowing users to
    type non-consecutive portions of the string to be matched.
    Adds the option fuzzy to config.def.h and the flag -F to dmenu which enable
    to turn fuzzy-matching off.
- **fuzzyhighlight**:  
    This patch make it so that fuzzy matches gets highlighted and is therefore
    meant to be used together with the patch fuzzymatch.
- **numbers**:  
    Adds text which displays the number of matched and total items in the top
    right corner of dmenu.
- **password**:  
    This patch add a flag (-P) which make dmenu not directly display the keyboard
    input, but instead replace it with dots. All data from stdin will be ignored.
- **embeddedAlpha**:  
    This patch fix the error with the -w flag
- **listfullwidth**:  
