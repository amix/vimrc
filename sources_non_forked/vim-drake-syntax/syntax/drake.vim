"
"  Vim syntax file for drake
"  Language: drake
"  Maintainer: Lars Yencken <lars@yencken.org>
"  Latest Revision: 2013-01-31
"

if exists("b:current_syntax")
  finish
endif

let b:current_syntax = ''
unlet b:current_syntax
syn include @Shell syntax/sh.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Python syntax/python.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Ruby syntax/ruby.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Clojure syntax/clojure.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @R syntax/r.vim


" Comments
syn match drakeComment ";.*$" contains=drakeTodo

" Variable definitions
syn match drakeSetVariable "^[a-zA-Z-_][a-zA-Z-_]*=[^,]*$" contains=drakeVariable
syn match drakeVariable "^[a-zA-Z-_][a-zA-Z-_]*" contained nextgroup=drakeSetIdentifier containedin=drakeVariable
syn match drakeSetIdentifier "=" nextgroup=drakeSetValue
syn match drakeSetValue ".*$" contained contains=drakeString
syn match drakeString "[^ ,]*" contained contains=drakeVariableRef containedin=drakeSetValue

" Method blocks
syn region drakeMethodBlock start="^[a-zA-Z-][a-zA-Z0-9-]*()" end="^$" contains=drakeMethodSignature
syn match drakeMethodSignature "^[a-zA-Z-][a-zA-Z0-9-]*" contained nextgroup=drakeMethodBraces
syn match drakeMethodBraces "()" contained nextgroup=drakeDefaultShBlock,drakeShBlock,drakePyBlock

" Variable references in strings
syn region drakeVariableRef matchgroup=VarBraces start='\$\[' end='\]' contained containedin=drakeString contains=drakeVariableName
syn match drakeVariableName "[a-zA-Z-_][a-zA-Z-_]*" contained containedin=drakeVariableRef

" Rule blocks
syn region drakeBlock start="[^<;, ][^<;, ]*\(, [^<;, ][^<;, ]*\)* <-" end="^$" contains=drakeRule
syn match drakeRule "[^<;, ].* <-\( [^[<; ][^<; ]*\)*" contains=drakeTargets nextgroup=drakeDefaultShBlock,drakeShBlock,drakePyBlock
syn match drakeTargets "[^<;, ][^<;, ]*\(, [^<;, ][^<;, ]*\)*" contained nextgroup=drakeRuleIdentifier contains=drakeString,drakeSourceSep
syn match drakeSources "\( ,?[^<[;, ][^<;, ]*\)*" contained contains=drakeString,drakeSourceSep
syn match drakeRuleIdentifier " <-" contained nextgroup=drakeSources
syn match drakeTargetSep ", " contained containedin=drakeTargets
syn match drakeSourceSep ", " contained containedin=drakeSources

" Todos in comments
syn keyword drakeTodo contained TODO NOTE FIXME XXX

hi link drakeComment Comment
hi link drakeTodo Todo
hi link drakeVariable	Identifier
hi link drakeSetIdentifier Delimiter
hi link drakeRuleIdentifier Delimiter
hi link drakeString String
hi link drakeVariableName Identifier
hi link VarBraces SpecialComment
hi link drakeTargetSep Delimiter
hi link drakeSourceSep Delimiter

" Embedded shell region in block
syn region drakeDefaultShBlock matchgroup=Snip start='^[ \t][ \t]*' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@Shell
syn region drakeShBlock matchgroup=Snip start=' \[shell\]$' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@Shell
syn region drakePyBlock matchgroup=Snip start=' \[python\]$' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@Python
syn region drakePyBlock matchgroup=Snip start=' \[ruby\]$' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@Ruby
syn region drakePyBlock matchgroup=Snip start=' \[clojure\]$' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@Clojure
syn region drakePyBlock matchgroup=Snip start=' \[R\]$' end='^$' containedin=drakeBlock,drakeMethodBlock contains=@R


" Embedded shell regions in strings
syn region shellBrackets matchgroup=SnipBraces start='\$(' end=')' containedin=drakeString contains=@Shell

hi link Snip SpecialComment
hi link SnipBraces SpecialComment
hi link drakeVariableRef Identifier
hi link drakeMethodSignature Identifier
hi link drakeMethodBraces SpecialComment

let b:current_syntax = 'drake'
