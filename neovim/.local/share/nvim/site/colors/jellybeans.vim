" colors/jellybeans.vim

"             __       _ _       _
"             \ \  ___| | |_   _| |__   ___  __ _ _ __  ___
"              \ \/ _ \ | | | | |  _ \ / _ \/ _  |  _ \/ __|
"           /\_/ /  __/ | | |_| | |_| |  __/ |_| | | | \__ \
"           \___/ \___|_|_|\__  |____/ \___|\____|_| |_|___/
"                          \___/

"               "A colorful, dark color scheme for Vim."

" Color Scheme Description: {{{1


" File:         jellybeans.vim
" URL:          github.com/nanotech/jellybeans.vim
" Scripts URL:  vim.org/scripts/script.php?script_id=2555
" Maintainer:   NanoTech (nanotech.nanotechcorp.net)
" Version:      1.7
" Last Change:  June 21st, 2019
" License:      MIT
" Contributors: {{{2
"               Andrew Wong (w0ng)
"               Benjamin R. Haskell (benizi)
"               Brian Marshall (bmars)
"               Daniel Herbert (pocketninja)
"               David Liang <bmdavll at gmail dot com>
"               Filipe Silva (ninrod)
"               Henry So, Jr. <henryso@panix.com>
"               Ihor Kalnytskyi (ikalnytskyi)
"               Joe Doherty (docapotamus)
"               Karl Litterfeldt (Litterfeldt)
"               Keith Pitt (keithpitt)
"               Mike Schreifels (schreifels)
"               Philipp Rustemeier (12foo)
"               Rafael Bicalho (rbika)
"               Rich Healey (richo)
"               Siwen Yu (yusiwen)
"               Tim Willis (willist)
"               Tom McLaughlin (tmcoma)
" }}}
"
" Copyright (c) 2009-2019 NanoTech {{{2
"
" Permission is hereby granted, free of charge, to any per‐
" son obtaining a copy of this software and associated doc‐
" umentation  files  (the “Software”), to deal in the Soft‐
" ware without restriction,  including  without  limitation
" the rights to use, copy, modify, merge, publish, distrib‐
" ute, sublicense, and/or sell copies of the Software,  and
" to permit persons to whom the Software is furnished to do
" so, subject to the following conditions:
"
" The above copyright notice  and  this  permission  notice
" shall  be  included in all copies or substantial portions
" of the Software.
"
" THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY
" KIND,  EXPRESS  OR  IMPLIED, INCLUDING BUT NOT LIMITED TO
" THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICU‐
" LAR  PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE  LIABLE  FOR  ANY  CLAIM,
" DAMAGES  OR OTHER LIABILITY, WHETHER IN AN ACTION OF CON‐
" TRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CON‐
" NECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

" }}}

" Colorscheme Intro: {{{1

set background=dark
scriptencoding utf-8
hi clear
if exists('syntax_on')
  syntax reset
endif
let colors_name = 'jellybeans'

if has('gui_running') || (has('termguicolors') && &termguicolors)
  let s:true_color = 1
else
  let s:true_color = 0
endif

if s:true_color || &t_Co >= 88
  let s:low_color = 0
else
  let s:low_color = 1
endif

" }}}

" Configuration Variables: {{{1

" - g:jellybeans_overrides          (default = {})
" - g:jellybeans_use_lowcolor_black (default = 0)
" - g:jellybeans_use_gui_italics    (default = 1)
" - g:jellybeans_use_term_italics   (default = 1)

if exists('g:jellybeans_overrides') " {{{
  let s:overrides = g:jellybeans_overrides
else
  let s:overrides = {}
endif
" }}}

if exists('g:jellybeans_use_lowcolor_black')
      \ && g:jellybeans_use_lowcolor_black
  let s:termBlack = 'Black'
else
  let s:termBlack = 'Grey'
endif

let s:background_color = '151515'

if has_key(s:overrides, 'background') && has_key(s:overrides['background'], 'guibg')
    let s:background_color = s:overrides['background']['guibg']
endif

" }}}

" Functions: {{{1

" Color approximation functions by Henry So, Jr. and David Liang " {{{2
" Added to jellybeans.vim by Daniel Herbert

if &t_Co == 88 " {{{3

  fun! s:grey_number(x) abort " {{{4
  " returns an approximate grey index for the given grey level
    if a:x < 23
      return 0
    elseif a:x < 69
      return 1
    elseif a:x < 103
      return 2
    elseif a:x < 127
      return 3
    elseif a:x < 150
      return 4
    elseif a:x < 173
      return 5
    elseif a:x < 196
      return 6
    elseif a:x < 219
      return 7
    elseif a:x < 243
      return 8
    else
      return 9
    endif
  endfun
  " }}}

  fun! s:grey_level(n) abort " {{{4
  " returns the actual grey level represented by the grey index
    if a:n == 0
      return 0
    elseif a:n == 1
      return 46
    elseif a:n == 2
      return 92
    elseif a:n == 3
      return 115
    elseif a:n == 4
      return 139
    elseif a:n == 5
      return 162
    elseif a:n == 6
      return 185
    elseif a:n == 7
      return 208
    elseif a:n == 8
      return 231
    else
      return 255
    endif
  endfun
  " }}}

  fun! s:grey_color(n) abort " {{{4
  " returns the palette index for the given grey index
    if a:n == 0
      return 16
    elseif a:n == 9
      return 79
    else
      return 79 + a:n
    endif
  endfun
  " }}}

  fun! s:rgb_number(x) abort " {{{4
  " returns an approximate color index for the given color level
    if a:x < 69
      return 0
    elseif a:x < 172
      return 1
    elseif a:x < 230
      return 2
    else
      return 3
    endif
  endfun
  " }}}

  fun! s:rgb_level(n) abort " {{{4
  " returns the actual color level for the given color index
    if a:n == 0
      return 0
    elseif a:n == 1
      return 139
    elseif a:n == 2
      return 205
    else
      return 255
    endif
  endfun
  " }}}

  fun! s:rgb_color(x, y, z) abort " {{{4
  " returns the palette index for the given R/G/B color indices
    return 16 + (a:x * 16) + (a:y * 4) + a:z
  endfun
  " }}}

" }}}

else " assuming &t_Co == 256 {{{3

  fun! s:grey_number(x) abort " {{{4
  " returns an approximate grey index for the given grey level
    if a:x < 14
      return 0
    else
      let l:n = (a:x - 8) / 10
      let l:m = (a:x - 8) % 10
      if l:m < 5
        return l:n
      else
        return l:n + 1
      endif
    endif
  endfun
  " }}}

  fun! s:grey_level(n) abort " {{{4
  " returns the actual grey level represented by the grey index
    if a:n == 0
      return 0
    else
      return 8 + (a:n * 10)
    endif
  endfun
  " }}}

  fun! s:grey_color(n) abort " {{{4
  " returns the palette index for the given grey index
    if a:n == 0
      return 16
    elseif a:n == 25
      return 231
    else
      return 231 + a:n
    endif
  endfun
  " }}}

  fun! s:rgb_number(x) abort " {{{4
  " returns an approximate color index for the given color level
    if a:x < 75
      return 0
    else
      let l:n = (a:x - 55) / 40
      let l:m = (a:x - 55) % 40
      if l:m < 20
        return l:n
      else
        return l:n + 1
      endif
    endif
  endfun
  " }}}

  fun! s:rgb_level(n) abort " {{{4
  " returns the actual color level for the given color index
    if a:n == 0
      return 0
    else
      return 55 + (a:n * 40)
    endif
  endfun
  " }}}

  fun! s:rgb_color(x, y, z) abort " {{{4
  " returns the palette index for the given R/G/B color indices
    return 16 + (a:x * 36) + (a:y * 6) + a:z
  endfun
  " }}}

endif

" }}}

" }}}

fun! s:color(r, g, b) abort " {{{2
" returns the palette index to approximate the given R/G/B color levels
  " map greys directly (see xterm's 256colres.pl)
  if &t_Co == 256 && a:r == a:g && a:g == a:b && a:r > 3 && a:r < 243
    return (a:r - 8) / 10 + 232
  endif

  " get the closest grey
  let l:gx = s:grey_number(a:r)
  let l:gy = s:grey_number(a:g)
  let l:gz = s:grey_number(a:b)

  " get the closest color
  let l:x = s:rgb_number(a:r)
  let l:y = s:rgb_number(a:g)
  let l:z = s:rgb_number(a:b)

  if l:gx == l:gy && l:gy == l:gz
    " there are two possibilities
    let l:dgr = s:grey_level(l:gx) - a:r
    let l:dgg = s:grey_level(l:gy) - a:g
    let l:dgb = s:grey_level(l:gz) - a:b
    let l:dgrey = (l:dgr * l:dgr) + (l:dgg * l:dgg) + (l:dgb * l:dgb)
    let l:dr = s:rgb_level(l:gx) - a:r
    let l:dg = s:rgb_level(l:gy) - a:g
    let l:db = s:rgb_level(l:gz) - a:b
    let l:drgb = (l:dr * l:dr) + (l:dg * l:dg) + (l:db * l:db)
    if l:dgrey < l:drgb
      " use the grey
      return s:grey_color(l:gx)
    else
      " use the color
      return s:rgb_color(l:x, l:y, l:z)
    endif
  else
    " only one possibility
    return s:rgb_color(l:x, l:y, l:z)
  endif
endfun
" }}}

fun! s:is_empty_or_none(str) abort " {{{2
  return empty(a:str) || a:str ==? 'NONE'
endfun
" }}}

fun! s:rgb(rgb) abort " {{{2
" returns the palette index to approximate the 'rrggbb' hex string
  if s:is_empty_or_none(a:rgb)
    return 'NONE'
  endif
  let l:r = ('0x' . strpart(a:rgb, 0, 2)) + 0
  let l:g = ('0x' . strpart(a:rgb, 2, 2)) + 0
  let l:b = ('0x' . strpart(a:rgb, 4, 2)) + 0
  return s:color(l:r, l:g, l:b)
endfun
" }}}

fun! s:prefix_highlight_value_with(prefix, color) abort " {{{2
  if s:is_empty_or_none(a:color)
    return 'NONE'
  else
    return a:prefix . a:color
  endif
endfun
" }}}

fun! s:remove_italic_attr(attr) abort " {{{2
  let l:attr = join(filter(split(a:attr, ','), "v:val !=? 'italic'"), ',')
  if empty(l:attr)
    let l:attr = 'NONE'
  endif
  return l:attr
endfun
" }}}

fun! s:X(group, fg, bg, attr, lcfg, lcbg) abort " {{{2
" sets the highlighting for the given group
  if s:low_color
    let l:cmd = 'hi '.a:group.
    \ ' ctermfg='.s:prefix_highlight_value_with('', a:lcfg).
    \ ' ctermbg='.s:prefix_highlight_value_with('', a:lcbg)
  else
    let l:cmd = 'hi '.a:group.
    \ ' guifg='.s:prefix_highlight_value_with('#', a:fg).
    \ ' guibg='.s:prefix_highlight_value_with('#', a:bg)
    if !s:true_color
      let l:cmd = l:cmd.
      \ ' ctermfg='.s:rgb(a:fg).
      \ ' ctermbg='.s:rgb(a:bg)
    endif
  endif

  let l:attr = s:prefix_highlight_value_with('', a:attr)

  if !exists('g:jellybeans_use_term_italics') || g:jellybeans_use_term_italics
    let l:cterm_attr = l:attr
  else
    let l:cterm_attr = s:remove_italic_attr(l:attr)
  endif

  if !exists('g:jellybeans_use_gui_italics') || g:jellybeans_use_gui_italics
    let l:gui_attr = l:attr
  else
    let l:gui_attr = s:remove_italic_attr(l:attr)
  endif

  let l:cmd = l:cmd.' gui='.l:gui_attr.' cterm='.l:cterm_attr
  exec l:cmd
endfun
" }}}

" }}}

" Highlighting: {{{1

" Bunch Of Vim Basics: {{{2

call s:X('Normal','e8e8d3',s:background_color,'','White','')
set background=dark
call s:X('CursorLine','','1c1c1c','','',s:termBlack)
call s:X('CursorColumn','','1c1c1c','','',s:termBlack)

" Match Parentheses: {{{3
" Some of Terminal.app's default themes have a cursor color
" too close to Jellybeans' preferred MatchParen background
" color to be easily distinguishable. Other terminals tend
" to use a brighter cursor color.
"
" Use a more distinct color in Terminal.app, and also in
" low-color terminals if the preferred background color is
" not available.
if !has('gui_running') && $TERM_PROGRAM ==? 'Apple_Terminal'
    let s:matchParenGuiFg = 'dd0093'
    let s:matchParenGuiBg = '000000'
else
    let s:matchParenGuiFg = 'ffffff'
    let s:matchParenGuiBg = '556779'
endif
if s:termBlack !=# 'Black'
    let s:matchParenTermFg = 'Magenta'
    let s:matchParenTermBg = ''
else
    let s:matchParenTermFg = ''
    let s:matchParenTermBg = s:termBlack
endif
call s:X('MatchParen',s:matchParenGuiFg,s:matchParenGuiBg,'bold',
      \ s:matchParenTermFg,s:matchParenTermBg)
" }}}

call s:X('TabLine','000000','b0b8c0','italic','',s:termBlack)
call s:X('TabLineFill','9098a0','','','',s:termBlack)
call s:X('TabLineSel','000000','f0f0f0','italic,bold',s:termBlack,'White')
call s:X('StatusLine','000000','dddddd','italic','','White')
call s:X('StatusLineNC','ffffff','403c41','italic','White','Black')
call s:X('Pmenu','ffffff','606060','','White',s:termBlack)
call s:X('PmenuSel','101010','eeeeee','',s:termBlack,'White')
call s:X('Visual','','404040','','',s:termBlack)
call s:X('Cursor',s:background_color,'b0d0f0','','','')
call s:X('LineNr','605958',s:background_color,'NONE',s:termBlack,'')
call s:X('CursorLineNr','ccc5c4','','NONE','White','')
call s:X('Comment','888888','','italic','Grey','')
call s:X('Todo','c7c7c7','','bold','White',s:termBlack)
call s:X('VertSplit','777777','403c41','',s:termBlack,s:termBlack)
call s:X('WildMenu','f0a0c0','302028','','Magenta','')
call s:X('Folded','a0a8b0','384048','italic',s:termBlack,'')
call s:X('FoldColumn','535D66','1f1f1f','','',s:termBlack)
call s:X('SignColumn','777777','333333','','',s:termBlack)
call s:X('ColorColumn','','000000','','',s:termBlack)
call s:X('Title','70b950','','bold','Green','')
call s:X('Constant','cf6a4c','','','Red','')
call s:X('Special','799d6a','','','Green','')
call s:X('Delimiter','668799','','','Grey','')
call s:X('String','99ad6a','','','Green','')
call s:X('StringDelimiter','556633','','','DarkGreen','')
call s:X('Identifier','c6b6ee','','','LightCyan','')
call s:X('Structure','8fbfdc','','','LightCyan','')
hi! link Operator Structure
hi! link Conceal Operator
call s:X('Function','fad07a','','','Yellow','')
call s:X('Statement','8197bf','','','DarkBlue','')
call s:X('PreProc','8fbfdc','','','LightBlue','')
call s:X('Type','ffb964','','','Yellow','')
call s:X('NonText','606060',s:background_color,'',s:termBlack,'')
call s:X('SpecialKey','444444','1c1c1c','',s:termBlack,'')
call s:X('Search','f0a0c0','302028','underline','Magenta','')
call s:X('Directory','dad085','','','Yellow','')
call s:X('ErrorMsg','','902020','','','DarkRed')
hi! link Error ErrorMsg
hi! link MoreMsg Special
call s:X('Question','65C254','','','Green','')
" }}}

" Spell Checking: {{{2
call s:X('SpellBad','','902020','underline','','DarkRed')
call s:X('SpellCap','','0000df','underline','','Blue')
call s:X('SpellRare','','540063','underline','','DarkMagenta')
call s:X('SpellLocal','','2D7067','underline','','Green')
" }}}

" Differences: {{{2
hi! link diffRemoved Constant
hi! link diffAdded String
" }}}

" VimDiff: {{{2
call s:X('DiffAdd','D2EBBE','437019','','White','DarkGreen')
call s:X('DiffDelete','40000A','700009','','DarkRed','DarkRed')
call s:X('DiffChange','','2B5B77','','White','DarkBlue')
call s:X('DiffText','8fbfdc','000000','reverse','Yellow','')
" }}}

" PHP: {{{2
hi! link phpFunctions Function
call s:X('StorageClass','c59f6f','','','Red','')
hi! link phpSuperglobal Identifier
hi! link phpQuoteSingle StringDelimiter
hi! link phpQuoteDouble StringDelimiter
hi! link phpBoolean Constant
hi! link phpNull Constant
hi! link phpArrayPair Operator
hi! link phpOperator Normal
hi! link phpRelation Normal
hi! link phpVarSelector Identifier
" }}}

" Python: {{{2
hi! link pythonOperator Statement
" }}}

" Ruby: {{{2
hi! link rubySharpBang Comment
call s:X('rubyClass','447799','','','DarkBlue','')
call s:X('rubyIdentifier','c6b6fe','','','Cyan','')
hi! link rubyConstant Type
hi! link rubyFunction Function
call s:X('rubyInstanceVariable','c6b6fe','','','Cyan','')
call s:X('rubySymbol','7697d6','','','Blue','')
hi! link rubyGlobalVariable rubyInstanceVariable
hi! link rubyModule rubyClass
call s:X('rubyControl','7597c6','','','Blue','')
hi! link rubyString String
hi! link rubyStringDelimiter StringDelimiter
hi! link rubyInterpolationDelimiter Identifier
call s:X('rubyRegexpDelimiter','540063','','','Magenta','')
call s:X('rubyRegexp','dd0093','','','DarkMagenta','')
call s:X('rubyRegexpSpecial','a40073','','','Magenta','')
call s:X('rubyPredefinedIdentifier','de5577','','','Red','')
" }}}

" Erlang: {{{2
hi! link erlangAtom rubySymbol
hi! link erlangBIF rubyPredefinedIdentifier
hi! link erlangFunction rubyPredefinedIdentifier
hi! link erlangDirective Statement
hi! link erlangNode Identifier
" }}}

" Elixir: {{{2
hi! link elixirAtom rubySymbol
" }}}

" JavaScript: {{{2
hi! link javaScriptValue Constant
hi! link javaScriptRegexpString rubyRegexp
hi! link javaScriptTemplateVar StringDelim
hi! link javaScriptTemplateDelim Identifier
hi! link javaScriptTemplateString String
" }}}

" CoffeeScript: {{{2
hi! link coffeeRegExp javaScriptRegexpString
" }}}

" Lua: {{{2
hi! link luaOperator Conditional
" }}}

" C: {{{2
hi! link cFormat Identifier
hi! link cOperator Constant
" }}}

" Objective-C/Cocoa: {{{2
hi! link objcClass Type
hi! link cocoaClass objcClass
hi! link objcSubclass objcClass
hi! link objcSuperclass objcClass
hi! link objcDirective rubyClass
hi! link objcStatement Constant
hi! link cocoaFunction Function
hi! link objcMethodName Identifier
hi! link objcMethodArg Normal
hi! link objcMessageName Identifier
" }}}

" Vimscript: {{{2
hi! link vimOper Normal
" }}}

" HTML: {{{2
hi! link htmlTag Statement
hi! link htmlEndTag htmlTag
hi! link htmlTagName htmlTag
" }}}

" XML: {{{2
hi! link xmlTag Statement
hi! link xmlEndTag xmlTag
hi! link xmlTagName xmlTag
hi! link xmlEqual xmlTag
hi! link xmlEntity Special
hi! link xmlEntityPunct xmlEntity
hi! link xmlDocTypeDecl PreProc
hi! link xmlDocTypeKeyword PreProc
hi! link xmlProcessingDelim xmlAttrib
" }}}

" Debugger.vim: {{{2
call s:X('DbgCurrent','DEEBFE','345FA8','','White','DarkBlue')
call s:X('DbgBreakPt','','4F0037','','','DarkMagenta')
" }}}

" Vim Indent Guides: {{{2
if !exists('g:indent_guides_auto_colors')
  let g:indent_guides_auto_colors = 0
endif
call s:X('IndentGuidesOdd','','232323','','','')
call s:X('IndentGuidesEven','','1b1b1b','','','')
" }}}

" Plugins: {{{2
hi! link TagListFileName Directory
call s:X('PreciseJumpTarget','B9ED67','405026','','White','Green')
" }}}

" Manual Overrides : {{{2
" For 256 Color Terminals
" Dark colors auto-map badly.
if !s:low_color
  hi StatusLineNC ctermbg=235
  hi Folded ctermbg=236
  hi DiffText ctermfg=81
  hi DbgBreakPt ctermbg=53
  hi IndentGuidesOdd ctermbg=235
  hi IndentGuidesEven ctermbg=234
endif
" }}}

" }}}

" User Overrides: {{{1

if !empty('s:overrides')

  fun! s:current_attr(group) abort " {{{2
    let l:synid = synIDtrans(hlID(a:group))
    let l:attrs = []
    for l:attr in ['bold', 'italic', 'reverse', 'standout', 'underline', 'undercurl']
      if synIDattr(l:synid, l:attr, 'gui') == 1
        call add(l:attrs, l:attr)
      endif
    endfor
    return join(l:attrs, ',')
  endfun
  " }}}

  fun! s:current_color(group, what, mode) abort " {{{2
    let l:color = synIDattr(synIDtrans(hlID(a:group)), a:what, a:mode)
    if l:color == -1
      return ''
    else
      return substitute(l:color, '^#', '', '')
    endif
  endfun
  " }}}

  fun! s:load_color_def(group, def) abort " {{{2
    call s:X(a:group, get(a:def, 'guifg', s:current_color(a:group, 'fg', 'gui')),
    \                 get(a:def, 'guibg', s:current_color(a:group, 'bg', 'gui')),
    \                 get(a:def, 'attr', s:current_attr(a:group)),
    \                 get(a:def, 'ctermfg', s:current_color(a:group, 'fg', 'cterm')),
    \                 get(a:def, 'ctermbg', s:current_color(a:group, 'bg', 'cterm')))
    if !s:low_color
      for l:prop in ['ctermfg', 'ctermbg']
        let l:override_key = '256'.l:prop
        if has_key(a:def, l:override_key)
          exec 'hi '.a:group.' '.l:prop.'='.a:def[l:override_key]
        endif
      endfor
    endif
  endfun
  " }}}

  fun! s:load_colors(defs) abort " {{{2
    for [l:group, l:def] in items(a:defs)
      if l:group ==# 'background'
        call s:load_color_def('LineNr', l:def)
        call s:load_color_def('NonText', l:def)
        call s:load_color_def('Normal', l:def)
      else
        call s:load_color_def(l:group, l:def)
      endif
      unlet l:group
      unlet l:def
    endfor
  endfun
  " }}}

  call s:load_colors(s:overrides)

  " Cleaning: {{{2
  delf s:load_colors
  delf s:load_color_def
  delf s:current_color
  delf s:current_attr
  " }}}

endif

" }}}

" Cleaning: {{{1

delf s:X
delf s:remove_italic_attr
delf s:prefix_highlight_value_with
delf s:rgb
delf s:is_empty_or_none
delf s:color
delf s:rgb_color
delf s:rgb_level
delf s:rgb_number
delf s:grey_color
delf s:grey_level
delf s:grey_number

" }}}
