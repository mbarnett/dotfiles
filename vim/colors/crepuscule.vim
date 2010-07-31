" Vim color scheme
"
" Name:         crepuscule.vim
" Maintainer:   Matthew Barnett <matt.barnett@gmail.com>
" Last Change:  22 Feb 2009 
" License:      public domain
" Version:      2.0
"
" Modified railscasts, originally created be Josh O'Rourke <jorourke23@gmail.com> 
"
" Note from railscasts.vim
" This theme is based on the Railscasts Textmate theme [1]. I used 
" Jo Vermeulen's "vibrantink" theme for Vim [2] as my template for 
" creating this theme.
"
" [1] http://railscasts.com/about 
" [2] http://www.vim.org/scripts/script.php?script_id=1794 

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "crepuscule"

if has("gui_running")
  hi link htmlTag                     xmlTag
  hi link htmlTagName                 xmlTagName
  hi link htmlEndTag                  xmlEndTag

  highlight Normal                    guifg=#EBE7E3   guibg=#393939
  highlight Cursor                    guifg=#000000   guibg=#FFFFFF
 
  highlight Comment                   guifg=#00B5E1   gui=italic
  highlight Constant                  guifg=#FF3A83
  highlight Define                    guifg=#FB8E2E
  highlight Error                     guifg=#FFFFFF   guibg=#990000
  highlight Function                  guifg=#FFC66D   gui=NONE
  highlight Identifier                guifg=#6D9CBE   gui=NONE
  highlight Include                   guifg=#CC7833   gui=NONE
  highlight Keyword                   guifg=#FB8E2E
  highlight Number                    guifg=#A5C261
  highlight PreProc                   guifg=#EBE7E3 
  highlight Search                    guibg=#FFFF00
  highlight Statement                 guifg=#FB8E2E   gui=NONE
  highlight String                    guifg=#55E439
  highlight Title                     guifg=#FFFFFF
  highlight Type                      guifg=#E65C4D   gui=NONE
  highlight Visual                    guibg=#5A647E

  highlight DiffAdd                   guifg=#EBE7E3    guibg=#144212
  highlight DiffDelete                guifg=#EBE7E3    guibg=#660000
  
  highlight rubyBlockParameter        guifg=#EBE7E3    guibg=#990000 
  highlight rubyClass                 guifg=#FF7D18
  highlight rubyConstant              guifg=#E65C4D
  highlight rubyInstanceVariable      guifg=#FF3300
  highlight rubyInterpolation         guifg=#EBE7E3    guibg=#5B5B5B
  highlight rubyLocalVariableOrMethod guifg=#FFA500
  highlight rubyPredefinedConstant    guifg=#E65C4D
  highlight rubyPseudoVariable        guifg=#FFC66D
  highlight rubyStringDelimiter       guifg=#A5C261
  
  highlight xmlTag                    guifg=#E8BF6A
  highlight xmlTagName                guifg=#E8BF6A
  highlight xmlEndTag                 guifg=#E8BF6A

  highlight NonText		      guifg=#1930D5

  highlight StatusLine		      guifg=#ebe7e3 guibg=#393939
  highlight LineNr                    guifg=#797979 guibg=#2D2D2D

  highlight Directory		      guifg=#00BBEA

endif
