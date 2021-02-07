" Vim Syntax file
" Language:     Portlable Game Notation
" Author:       Marco Cantoro <marco.cantoro92@outlook.it>
" Filenames:    *.pgn

" Syntax Guards: {{{1

if exists("b:current_syntax")
  finish
endif

" }}}

" Syntax Definition: {{{1

syn case ignore

" Keywords: {{{2
syn keyword pgnLabel Event Site Date Round White Black Result
syn keyword pgnLabel Annotator PlyCount TimeControl Time Termination Mode FEN
syn keyword pgnLabel UTCDate UTCTime EventDate SourceDate EventCountry
syn keyword pgnLabel Variant ECO Opening
syn keyword pgnLabel WhiteElo BlackElo WhiteTeam BlackTeam
syn keyword pgnLabel WhiteClock BlackClock WhiteTeamCountry BlackTeamCountry
syn keyword pgnLabel WhiteRatingDiff BlackRatingDiff
" }}}

" Matches: {{{2
syn match pgnMoveNumber /[0-9]*\./
syn match pgnSymbol /[x\+\#]/
syn match pgnResult /[0-2]\/*[0-2]*[-][0-2]\/*[0-2]*/
syn match pgnComment "\v;.*$"
" }}}

" Regions: {{{2
syn region pgnComment start=/{/ end=/}/
syn region pgnString start=/"/ end=/"/ contained
syn region pgnTypedef start=/\[/ end=/\]/ contains=pgnString,pgnLabel
" }}}

" }}}

" Highlighting Links: {{{1
hi link pgnComment          Comment
hi link pgnString           String
hi link pgnResult           Number
hi link pgnMoveNumber       Identifier
hi link pgnLabel            Label
hi link pgnTypedef          Typedef
hi link pgnSymbol           Special
" }}}

" Syntax Closure: {{{1
let b:current_syntax = "pgn"
" }}}
