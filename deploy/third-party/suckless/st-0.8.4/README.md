st - simple terminal
--------------------
st is a simple terminal emulator for X which sucks less.


Requirements
------------
In order to build st you need the Xlib header files.


Installation
------------
Edit config.mk to match your local setup (st is installed into
the /usr/local namespace by default).

Afterwards enter the following command to build and install st (if
necessary as root):

    make clean install


Running st
----------
If you did not install st with make clean install, you must compile
the st terminfo entry with the following command:

    tic -sx st.info

See the man page for additional details.

Credits
-------
Based on Aurélien APTEL <aurelien dot aptel at gmail dot com> bt source code.

Patches
-------
Applied patches in the order in which have been applied

- **alpha**: for background transparency
- **scrollbackkeyboard**: for terminal scrolling with Shift+PgUp/PgDown  
- **scrollbackmouse**: for terminal scrolling with Shift+Mouse4/Mouse5  
- **scrollbackmousealtscreen**: for terminal scrolling with Mouse4/Mouse5 only when not in MODE_ALTSCREEN  
- **palette**: for changing on the spot the terminal color theme  
- **desktop-entry**: linux desktop file  
- **working-dir**: for specify the initial working directory as an argument to st  
- **boxdraw**: custom rendering of lines/block/braille characters for gapless alignment  
- **ligature-boxdraw**: for rendering ligature  
- **copyurl**: select and copy the last URL displayed with CTRL+Shift+L  
- **hidecursor**: hide the X cursor when typing  
- **cyclefonts**: cycle between multiple fonts with CTRL+Shift+F  
- **bold is not bright**: not render bold text as bright  
- **iso 14755**: dmenu popup to enter unicode codepoint with CTRL+Shift+U  
- **newterm**: spawn a new terminal with the same cwd with CTRL+Shift+Enter  
- **keyboard-select**: to select and copy text with keyboard shortcuts, press CTRL+Shift+space to enter the *keyboard-select* mode  
