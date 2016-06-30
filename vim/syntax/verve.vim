" Vim syntax file
" Language:	verve
" Current Maintainer:	tadeuzagallo (https://github.com/tadeuzagallo)

syn keyword verveLanguageKeywords if else interface implementation extern virtual type let import from as match


" Integer with - + or nothing in front
syn match verveNumber '\d\+'
syn match verveNumber '[-+]\d\+'

syn region verveString	start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell extend
syn match	verveChar "'[^']'"

syn match verveOperator "\v(\+|-|\/|\*|\=|\!|\<|\>|#)+"

syn match blueArrow "->"
syn match redArrow "->"
syn match redColon ":"

syn match verveTypePrefix "\v:\s*" contains=redColon nextgroup=verveTypes
syn match verveTypePrefix2 "\v-\>\s*" contains=redArrow nextgroup=verveTypes


syn match verveTypes contained "\v[A-Za-z_]+"
syn match verveTemplate /\v\<[^>]+\>+/
syn match verveDecl /\v\([^:]*\)\s*-\>[^,){]+/


syn match redFn "fn"
syn match ignoreFnDecl /\vfn\s+[^(]+\(/ contains=redFn

syn match ignoreForceIdentifier /\v`[^`]+`/ contains=redFn

syn keyword verveTodo contained TODO NOTE
syn region	verveComment	start="//" skip="\\$" end="$" keepend contains=verveTodo
syn region verveMultilineComment	start="/\*" end="\*/" contains=verveTodo,@Spell extend

let b:current_syntax = "verve"

hi def link verveLanguageKeywords Statement
hi def link verveTodo Todo
hi def link verveMultilineComment Comment
hi def link verveComment Comment
hi def link verveTypes Identifier
hi def link verveNumber Number
hi def link verveString String
hi def link verveChar Character

hi def link verveOperator Operator
hi def link redColon Operator
hi def link redArrow Operator
hi def link redFn Operator

hi def link blueArrow Identifier
hi def link verveDecl Identifier
hi def link verveTemplate Identifier
