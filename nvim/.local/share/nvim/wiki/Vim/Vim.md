# [Vim CheatSheet][VimSite]

Detailed [documentation][VimDoc]

 ## Table Of Contents

<a name="ch01"></a>
1. [ Cursor Movements ](#moves)
<a name="ch02"></a>
2. [ Bookmarks ](#books)
<a name="ch03"></a>
3. [ Insert Mode ](#insert)
<a name="ch04"></a>
4. [ Editing ](#edit)
<a name="ch05"></a>
5. [ Deleting Text ](#delete)
<a name="ch06"></a>
6. [ Copying and Moving Text ](#copy)
<a name="ch07"></a>
7. [ Macros ](#macro)
<a name="ch08"></a>
8. [ Visual Mode ](#visual)
<a name="ch09"></a>
9. [ Visual Mode Commands ](#vis_com)
<a name="ch10"></a>
10. [ Visual Mode Shortcuts ](#vis_short)
<a name="ch11"></a>
11. [ Spelling ](#spell)
<a name="ch12"></a>
12. [ Exiting ](#exit)
<a name="ch13"></a>
13. [ Search/Replace ](#search)
<a name="ch14"></a>
14. [ Multiple Files ](#multiple)
<a name="ch15"></a>
15. [ Windows ](#windows)
<a name="ch16"></a>
16. [ Quickfix Window ](#quick_win)
<a name="ch17"></a>
17. [ Programming ](#program)

<a name="moves"></a>
## [Cursor Movements](#ch01)

| Key       | Movement                                                                 |
| :---      | :---                                                                     |
| h         | move left                                                                |
| j         | move down                                                                |
| k         | move up                                                                  |
| l         | move right                                                               |
| w         | jump by start of words (punctuation considered words)                    |
| W         | jump by words (spaces separate words)                                    |
| e         | jump to end of words (punctuation considered words)                      |
| E         | jump to end of words (no punctuation)                                    |
| b         | jump backward by words (punctuation considered words)                    |
| B         | jump backward by words (no punctuation)                                  |
| ge        | as e but backward                                                        |
| gE        | as E but backward                                                        |
| g         | jump backward to end of words                                            |
| 0         | (zero) start of line                                                     |
| ^         | first non-blank character of line                                        |
| $         | end of line                                                              |
| -         | move line upwards, on the first non blank character                      |
| +         | move line downwards, on the first non blank character                    |
| <         | >             move line downwards, on the first non blank character      |
| gg        | go to first line                                                         |
| G         | go to last line                                                          |
| nG        | go To line n                                                             |
| )         | move the cursor forward to the next sentence.                            |
| (         | move the cursor backward by a sentence.                                  |
| {         | move the cursor a paragraph backwards                                    |
| }         | move the cursor a paragraph forwards                                     |
| ]         | move the cursor a section forwards or to the next {                      |
| [         | move the cursor a section backwards or the previous {                    |
| Ctrl-f    | move the cursor forward by a screen of text                              |
| Ctrl-b    | move the cursor backward by a screen of text                             |
| Ctrl-u    | move the cursor up by half a screen                                      |
| Ctrl-d    | move the cursor down by half a screen                                    |
| H         | move the cursor to the top of the screen.                                |
| M         | move the cursor to the middle of the screen.                             |
| L         | move the cursor to the bottom of the screen.                             |
| f         | search line forward for 'x'                                              |
| F         | search line backward for 'x'                                             |
| t         | search line forward before 'x'                                           |
| T         | search line backward before 'x'                                          |
| ;         | repeat this searching motion                                             |
| Ctrl-E    | scroll window [count] lines downwards in the buffer                      |
| Ctrl-Y    | scroll window [count] lines upward in the buffer                         |
| z<CR>     | Redraw, line [count] at top of window, and put curson at first non-blank |
| zt        | like z<CR> but leave the cursor in the same column                       |
| z{height} | Redraw, make window {height} lines tall                                  |
| z.        | Redraw, line [count] at center of window.                                |
| zz        | Like z. but leave cursor in the same column                              |
| z-        | Redraw, line [count] at bottom of window.                                |
| zb        | Like z- but leave cursor in the same column                              |


<a name="books"></a>
## [Bookmarks](#ch02)

| Key  | Action                                                 |
| :--- | :---                                                   |
| :    | list all the current marks                             |
| m    | make a bookmark named a at the current cursor position |
| `    | go to position of bookmark a                           |
| '    | go to the line with bookmark a                         |
| `    | go to the line that you last edited                    |

<a name="insert"></a>
## [Insert Mode](#ch03)

| Key    | Action                                      |
| :---   | :---                                        |
| i      | start insert mode at cursor                 |
| I      | insert at the beginning of the line         |
| a      | append after the cursor                     |
| A      | append at the end of the line               |
| o      | open (append) blank line below current line |
| O      | open blank line above current line          |
| Esc    | exit insert mode                            |
| ^r     | insert text from a register                 |
| ^a     | insert text from register '.'               |
| ^p     | completion menu                             |
| ^x     | special "completion mode" submode of insert |
| - - ^] | tag                                         |
| - - ^p | pull from previous context                  |
| - - ^n | pull from next context                      |
| - - ^f | file completion                             |
| - - ^l | line                                        |
| - - ^o | omnicompletion                              |

<a name="edit"></a>
## [Editing](#ch04)

| Key         | Action                                                         |
| :---        | :---                                                           |
| `r`         | replace a single character (does not use insert mode)          |
| `R`         | enter Insert mode, replacing characters rather than inserting  |
| `gJ`        | join lines and avoid leaving blank space between them          |
| `J`         | join line below to the current one                             |
| `cc`        | change (replace) an entire line                                |
| `cw`        | change (replace) to the end of word                            |
| `C`         | change (replace) to the end of line                            |
| `s`         | delete character at cursor and substitute text                 |
| `S`         | delete line at cursor and substitute text (same as cc)         |
| `u`         | undo                                                           |
| `CTRL-r`    | redo                                                           |
| `.`         | repeat last command                                            |
| `~`         | switch case                                                    |
| `g~iw`      | switch case of current word                                    |
| `gUiw`      | make current word uppercase                                    |
| `guiw`      | make current word lowercase                                    |
| `gU$`       | make uppercase until end of line                               |
| `gu$`       | make lowercase until end of line                               |
| `>>`        | indent line one column to right                                |
| `<<`        | indent line one column to left                                 |
| `==`        | auto-indent current line                                       |
| `=G`        | auto indent the whole file from the cursor position            |
| `:#,#<<`    | indent the corresponding number of lines                       |
| `:#,#>>`    | outdent the corresponding number of lines                      |
| `xp`        | transpose two letters (delete and paste, technically)          |
| `ddp`       | swap current line with next                                    |
| `ddkp`      | swap current line with previous                                |
| `:%retab`   | fix spaces / tabs issues in whole file                         |
| `:r [name]` | insert the file [name] below the cursor.                       |
| `:r !{cmd}` | execute {cmd} and insert its standard output below the cursor. |

<a name="delete"></a>
## [Deleting Text](#ch05)

| Key       | Action                            |
| :---      | :---                              |
| `x`         | delete current character          |
| `X`         | delete previous character         |
| `dw`        | delete the current word           |
| `dd`        | delete (cut) a line               |
| `D`         | delete from cursor to end of line |
| `:[range]d` | delete [range] lines              |

<a name="copy"></a>
## [Copying and Moving Text](#ch06)

| Key            | Action                                                      |
| :---           | :---                                                        |
| `yw`           | yank word                                                   |
| `yy`           | yank (copy) a line                                          |
| `2yy`          | yank 2 lines                                                |
| `y$`           | yank to end of line                                         |
| `p`            | put (paste) the clipboard after cursor/current line         |
| `P`            | put (paste) before cursor/current line                      |
| `:set`         | paste          avoid unexpected effects in pasting          |
| `:registers`   | display the contents of all registers                       |
| `"xyw`         | yank word into register x                                   |
| `"xyy`         | yank line into register x                                   |
| `:[range]y x`  | yank [range] lines into register x                          |
| `"xp`          | put the text from register x after the cursor               |
| `"xP`          | put the text from register x before the cursor              |
| `"xgp`         | just like "p", but leave the cursor just after the new text |
| `"xgP`         | just like "P", but leave the cursor just after the new text |
| `:[line]put x` | put the text from register x after [line]                   |

<a name="macro"></a>
## [Macros](#ch07)

| Key  | Action                    |
| :--- | :---                      |
| `qa` | start recording macro 'a' |
| `q`  | end recording macro       |
| `@a` | replay macro 'a'          |
| `@:` | replay last command       |

<a name="visual"></a>
## [Visual Mode](#ch08)

| Key      | Action                                                          |
| :---     | :---                                                            |
| `v`      | start visual mode, mark lines, then do command (such as y-yank) |
| `V`      | start line wise visual mode                                     |
| `o`      | move to other end of marked area                                |
| `U`      | upper case of marked area                                       |
| `CTRL-v` | start visual block mode                                         |
| `gv`     | repeat the last visual block selection                          |
| `O`      | move to other corner of block                                   |
| `aw`     | mark a word                                                     |
| `ab`     | a () block (with braces)                                        |
| `ab`     | a {} block (with brackets)                                      |
| `ib`     | inner () block                                                  |
| `ib`     | inner {} block                                                  |
| `Esc`    | exit visual mode                                                |

<a name="vis_com"></a>
## [Visual Mode Commands](#ch09)

| Key  | Action                       |
| :--- | :---                         |
| `>`  | shift right                  |
| `<`  | shift left                   |
| `c`  | change (replace) marked text |
| `y`  | yank (copy) marked text      |
| `d`  | delete marked text           |
| `~`  | switch case                  |

<a name="vis_short"></a>
## [Visual Mode Shortcuts](#ch10)

| Key   | Action                             |
| :---  | :---                               |
| `v%`  | selects matching parenthesis       |
| `vi{` | selects matching curly brace       |
| `vi"` | selects text between double quotes |
| `vi'` | selects text between single quotes |

<a name="spell"></a>
## [Spelling](#ch11)

| Key   | Action                   |
| :---  | :---                     |
| `]s`  | next misspelled word     |
| `[s`  | previous misspelled word |
| `zg`  | add word to wordlist     |
| `zug` | undo last add word       |
| `z=`  | suggest word             |

<a name="exit"></a>
## [Exiting](#ch12)

| Key             | Action                                              |
| :---            | :---                                                |
| `:q`            | quit Vim. This fails when changes have been made.   |
| `:q!`           | quit without writing.                               |
| `:cq`           | quit always, without writing.                       |
| `:wq`           | write the current file and exit.                    |
| `:wq!`          | write the current file and exit always.             |
| `:wq {file}`    | write to {file}. Exit if not editing the last       |
| `:wq! {file}`   | write to {file} and exit always.                    |
| `:[range]wq[!]` | same as above, but only write the lines in [range]. |
| `ZZ`            | write current file, if modified, and exit.          |
| `ZQ`            | quit current file and exit (same as ":q!").         |

<a name="search"></a>
## [Search/Replace](#ch13)

| Key              | Action                                                     |
| :---             | :---                                                       |
| `/pattern`       | search for pattern                                         |
| `?pattern`       | search backward for pattern                                |
| `n`              | repeat search in same direction                            |
| `N`              | repeat search in opposite direction                        |
| `*`              | search forward, word under cursor                          |
| `#`              | search backward, word under cursor                         |
| `g*`             | search forward without boundaries (match also parts)       |
| `g#`             | search backward without boundaries                         |
| `set ic`         | ignore case: turn on                                       |
| `set noic`       | ignore case: turn off                                      |
| `:s/old/new/`    | change the first occurrence in the line                    |
| `:s/old/new/g`   | change every occurence in the line                         |
| `#,#s/old/new/g` | change every occurence between the selected lines          |
| `:%s/old/new/g`  | replace all old with new throughout file                   |
| `:%s/old/new/gc` | replace all old with new throughout file with confirmation |
| `:argdo %s/old/new/gc | wq` | open multiple files and run this command to replace old with new in every file with confirmation, save and quit 

<a name="multiple"></a>
## [Multiple Files](#ch14)

| Key                | Action                                            |
| :---               | :---                                              |
| `:e filename`      | edit a file in a new buffer                       |
| `:tabe filename`   | edit a file in a new tab (Vim7, gVim)             |
| `:ls`              | list all buffers                                  |
| `:bn`              | go to next buffer                                 |
| `:bp`              | go to previous buffer                             |
| `:bd`              | delete a buffer (close a file)                    |
| `:b1`              | show buffer 1                                     |
| `:b vimrc`         | show buffer whose filename begins with "vimrc"    |
| `:bufdo <command>` | run 'command(s)' in all buffers                   |
| `:[range]bufdo`    | <command> run 'command(s)' for buffers in 'range' |

<a name="windows"></a>
## [Windows](#ch15)

| Key        | Action                   |
| :---       | :---                     |
| `:sp f`    | split open f             |
| `:vsp f`   | vsplit open f            |
| `CTRL-w s` | split windows            |
| `CTRL-w w` | switch between windows   |
| `CTRL-w q` | quit a window            |
| `CTRL-w v` | split windows vertically |
| `CTRL-w x` | swap windows             |
| `CTRL-w h` | left window              |
| `CTRL-w j` | down window              |
| `CTRL-w k` | up window                |
| `CTRL-w l` | right window             |
| `CTRL-w +` | increase window height   |
| `CTRL-w -` | decrease window height   |
| `CTRL-w <` | increase window width    |
| `CTRL-w >` | decrease window width    |
| `CTRL-w =` | equal window             |
| `CTRL-w o` | close other windows      |

<a name="quick_win"></a>
## [Quickfix Window](#ch16)

| Key         | Action                         |
| :---        | :---                           |
| `copen`     | open quickfix window           |
| `cclose`    | close quickfix window          |
| `cc [nr]`   | display error [nr]             |
| `cfirst`    | display the first error        |
| `clast`     | display the last error         |
| `[count]cn` | display [count] next error     |
| `[count]cp` | display [count] previous error |

<a name="program"></a>
## [Programming](#ch17)

| Key      | Action                                                                      |
| :---     | :---                                                                        |
| `!`      | execute eternal shell commands                                              |
| `%`      | show matching brace, bracket, or parenthese                                 |
| `gf`     | edit the file whose name is under or after the cursor                       |
| `gd`     | when the cursor is on a local variable or function, jump to its declaration |
| `''`     | return to the line where the cursor was before the latest jump              |
| `gi`     | return to insert mode where you inserted text the last time                 |
| `CTRL-o` | move to previous position you were at                                       |
| `CTRL-i` | move to more recent position you were at                                    |
| `q:`     | access command line history                                                 |
| `Q`      | enter exMode                                                                |
| `gQ`     | enter in exMode in NeoVim (used to regex search and replace commands)       |

[//]: # (Reference-Links)

[VimSite]: http://www.vim.org/
[VimDoc]: https://vim.sourceforge.io/docs.php
