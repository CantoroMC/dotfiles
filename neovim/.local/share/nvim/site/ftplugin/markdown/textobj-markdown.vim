" textobj-markdown - Text objects for markdown
" File:          textobj-markdown.vim
" Author:        coachshea
" Maintainer:    CantoroMC
" Description:   Text objects user for markdown
" Last Modified: settembre 07, 2020

" Section: Plugin Guards
if exists('g:loaded_textobj_markdown')
  finish
endif
let g:loaded_textobj_markdown = 1


" Section: Text Object Wrapping
call textobj#user#plugin('markdown', {
      \ 'chunk': {
      \   'select-a-function': 'textobj#markdown#chunks#af',
      \   'select-a'         : '<buffer>af',
      \   'select-i-function': 'textobj#markdown#chunks#if',
      \   'select-i'         : '<buffer>if',
      \   'pattern'          : '```\S',
      \   'move-n'           : '<buffer>]fs',
      \   'move-p'           : '<buffer>[fs',
      \   'region-type'      : 'V',
      \ },
      \ 'Bchunk': {
      \   'select-a-function': 'textobj#markdown#chunks#aF',
      \   'select-a'         : '<buffer>aF',
      \   'select-i-function': 'textobj#markdown#chunks#iF',
      \   'select-i'         : '<buffer>iF',
      \   'pattern'          : '```$',
      \   'move-n'           : '<buffer>]fe',
      \   'move-p'           : '<buffer>[fe',
      \   'region-type'      : 'V',
      \ },
      \ 'text': {
      \   'select-a'         : '<buffer>am',
      \   'select-a-function': 'textobj#markdown#chunks#am',
      \   'select-i'         : '<buffer>im',
      \   'select-i-function': 'textobj#markdown#chunks#im',
      \   'move-n'           : '<buffer>]ms',
      \   'move-n-function'  : 'textobj#markdown#chunks#n',
      \   'move-p'           : '<buffer>[ms',
      \   'move-p-function'  : 'textobj#markdown#chunks#p',
      \   'region-type'      : 'V',
      \ },
      \ 'Btext': {
      \   'select-a'         : '<buffer>aM',
      \   'select-a-function': 'textobj#markdown#chunks#aM',
      \   'select-i'         : '<buffer>iM',
      \   'select-i-function': 'textobj#markdown#chunks#iM',
      \   'move-n'           : '<buffer>]me',
      \   'move-n-function'  : 'textobj#markdown#chunks#N',
      \   'move-p'           : '<buffer>[me',
      \   'move-p-function'  : 'textobj#markdown#chunks#P',
      \   'region-type'      : 'V',
      \ },
      \ })
