" Vim syntax file
" Language:	Ceos
" Current Maintainer:	tadeuzagallo (https://github.com/tadeuzagallo)

syn keyword ceosLanguageKeywords if else interface implementation extern

syn keyword ceosTodo contained TODO NOTE
syn region	ceosComment	start="//" skip="\\$" end="$" keepend contains=ceosTodo
syn region ceosMultilineComment	start="/\*" end="\*/" contains=ceosTodo,@Spell extend


" Integer with - + or nothing in front
syn match ceosNumber '\d\+'
syn match ceosNumber '[-+]\d\+'

syn region ceosString	start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell extend
syn match	ceosChar "'[^']'"

syn match ceosTypePrefix "\v:\s*" nextgroup=ceosTypes

syn match ceosTypes contained "\v[A-Za-z]+"
syn region ceosTemplate start="<" end=">" keepend contains=ceosTypes
syn match ceosDecl /\v\([^:)']+\)\s*:/me=e-1 contains=ceosTypes

let b:current_syntax = "ceos"

hi def link ceosLanguageKeywords Statement
hi def link ceosTodo Todo
hi def link ceosMultilineComment Comment
hi def link ceosComment Comment
hi def link ceosTypes Identifier
hi def link ceosNumber Number
hi def link ceosString String
hi def link ceosChar Character
hi def link ceosOperator Operator
