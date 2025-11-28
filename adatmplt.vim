" vim syntax file
" Language: Ada template
" Maintainer: Jan Tomassi <jtomassi@fbk.eu>
" Last Change: 2024-10-26

" Quit when a (custom) syntax file was already loaded
"" if exists("b:current_syntax")
""     finish
"" endif

" let s:cpo_save = $cpo
" set cpo&vim

syn case match
syn sync fromstart
set foldmethod=syntax
set conceallevel=1

syn match atTag         "@_.\{-}_@"
" =, /=, >, >=, <, <=, and, or, xor, in, not, &
syn match atConditionOP   "\(=\|/=\|>\|>=\|<\|<=\|and\|or\|xor\|in\|not\|&\)" contained


syn match  atIfH    "\(@@IF@@\s\+\)\@<=.*$"    contains=atConditionOP,atTag contained 
syn match  atElsifH "\(@@ELSIF@@\s\+\)\@<=.*$" contains=atConditionOP,atTag contained 
syn match  atElseE  "\(@@ELSE@@\)\@<=.*$"      contains=NONE

syn region atIf    transparent matchgroup=atCond start="\s*@@IF@@"    end="\s*@@END_IF@@"   fold contains=ALLBUT,atConditionOP,atElsifH,atElseE,@InlineHilight,@SetHilight,@ExtendHilight
syn region atElse  transparent matchgroup=atCond start="\s*@@ELSE@@"  end="\s*@@END_IF@@"me=e-10 contains=ALLBUT,atConditionOP,atElse,atElsif,atIfH,atElsifH,@InlineHilight,@SetHilight,@ExtendHilight
syn region atElsif transparent matchgroup=atCond start="\s*@@ELSIF@@" end="\s*@@END_IF@@"me=e-10 end="\s*@@ELSIF@@"me=e-9 end="\s*@@ELSE@@"me=e-8 contains=ALLBUT,atConditionOP,atIfH,atElseE,@InlineHilight,@SetHilight,@ExtendHilight

syn region atInline matchgroup=atStmt start="\s*@@INLINE@@" start="\s*@@INLINE\(\(([^()\\]*\%(\\.[^()\\]*\)*)\)\{1}\|\(([^()\\]*\%(\\.[^()\\]*\)*)\)\{3}\)@@" end="\s*@@END_INLINE@@" fold contains=ALLBUT,atConditionOP,@IfHilight,@SetHilight,@ExtendHilight
syn match  atInlineE "\s*@@INLINE\%(([^()\\]*\%(\\.[^()\\]*\)*)\)\{2}@@"

syn region atTable matchgroup=atStmt start=/\s*@@TABLE\('\(REVERSE\|TERMINATE_SECTIONS\|TERSE\|ALIGN_ON(\%(\%("[^"\\]*\%(\\.[^"\\]*\)*",\s*\)*\%("[^"\\]*\%(\\.[^"\\]*\)*"\)\))\)\)*@@/ end="\s*@@END_TABLE@@" fold contains=ALLBUT,atConditionOP,@IfHilight,@InlineHilight,@SetHilight,@ExtendHilight
syn match  atTableE1 /\(@@TABLE\('\(REVERSE\|TERMINATE_SECTIONS\|TERSE\|ALIGN_ON(\%(\%("[^"\\]*\%(\\.[^"\\]*\)*",\s*\)*\%("[^"\\]*\%(\\.[^"\\]*\)*"\)\))\)\)*@@\)\@<=.\+/
syn match  atTableE2 /\(@@END_TABLE@@\)\@<=.\+/

syn region atMacro matchgroup=atStmt start=/\s*@@MACRO(.\{-})@@/ end=/\s*@@END_MACRO@@/ fold contains=ALLBUT,atConditionOP,@IfHilight,@InlineHilight,@SetHilight,@ExtendHilight
syn match  atMacroE1 /\(@@MACRO(.\{-})@@\)\@<=.+/
syn match  atMacroE1 /\(@@END_MACRO@@\)\@<=.+/

syn region atInclude matchgroup=atStmt start="\s*@@INCLUDE@@" end="$" contains=atTag,atComment

syn region atSet matchgroup=atStmt start="@@SET@@" end="$" contains=atSetH1
syn match  atSetH1 /\(@@SET@@\s\+\)\@<=\w*/ contained nextgroup=atSetH2 
syn match  atSetH2 /\s=/ contained nextgroup=atSetH3 
syn match  atSetH3 /.*/  contained 

syn region atExtend matchgroup=atStmt start="@@EXTENDS@@"      end="@@END_EXTENDS@@" fold contains=ALLBUT,atConditionOP,@IfHilight,@InlineHilight,@SetHilight
syn region atBlock  matchgroup=atStmt start="@@BLOCK(.\{-})@@" end="@@END_BLOCK@@"   fold contains=ALLBUT,atConditionOP,@IfHilight,@InlineHilight,@SetHilight

syn region atComment start="@@--" end="$" 
syn match  atCommentEmpty "@@--\s*$" conceal

syn cluster IfHilight     add=atIfH,atElsifH,atElseE
syn cluster InlineHilight add=atInlineE
syn cluster SetHilight    add=atSetH1,atSetH2,atSetH3
syn cluster ExtendHilight add=atSetH1,atSetH2,atSetH3

hi def link atComment     Comment
hi def link atStmt        Statement
hi def link atCond        Conditional

hi def link atTag         Identifier
hi def link atSetH1       Identifier

hi def link atConditionOP Operator
hi def link atSetH2       Operator

hi def link atSetH3       String

hi def link atElseE       Error
hi def link atInlineE     Error
hi def link atTableE1     Error
hi def link atTableE2     Error
hi def link atMacroE1     Error
hi def link atMacroE1     Error
