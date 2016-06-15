" Vim syntax file
" Language:	Ceos
" Current Maintainer:	tadeuzagallo (https://github.com/tadeuzagallo)

syn keyword ceosLanguageKeywords if else interface implementation extern virtual type let


" Integer with - + or nothing in front
syn match ceosNumber '\d\+'
syn match ceosNumber '[-+]\d\+'

syn region ceosString	start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell extend
syn match	ceosChar "'[^']'"

syn match ceosOperator "\v(\+|-|\/|\*|\=|\!|\<|\>)+"

syn match blueArrow "->"
syn match redArrow "->"
syn match redColon ":"

syn match ceosTypePrefix "\v:\s*" contains=redColon nextgroup=ceosTypes
syn match ceosTypePrefix2 "\v-\>\s*" contains=redArrow nextgroup=ceosTypes


syn match ceosTypes contained "\v[A-Za-z]+"
syn match ceosTemplate /\v\<[^>]+\>+/
syn match ceosDecl /\v\([^(:)]*\)\s*-\>[^,){]+/


syn match redFn "fn"
syn match ignoreFnDecl /\vfn\s+[^(]+\(/ contains=redFn

syn match ignoreForceIdentifier /\v`[^`]+`/ contains=redFn

syn keyword ceosTodo contained TODO NOTE
syn region	ceosComment	start="//" skip="\\$" end="$" keepend contains=ceosTodo
syn region ceosMultilineComment	start="/\*" end="\*/" contains=ceosTodo,@Spell extend

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
hi def link redColon Operator
hi def link redArrow Operator
hi def link redFn Operator

hi def link blueArrow Identifier
hi def link ceosDecl Identifier
hi def link ceosTemplate Identifier
