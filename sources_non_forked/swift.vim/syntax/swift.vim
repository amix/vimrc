" File: swift.vim
" Author: Keith Smiley
" Description: Runtime files for Swift
" Last Modified: June 15, 2014

if exists("b:current_syntax")
  finish
endif

" Comments
" Shebang
syntax match swiftShebang "\v#!.*$"

" Comment contained keywords
syntax keyword swiftTodos contained TODO XXX FIXME NOTE
syntax keyword swiftMarker contained MARK

" In comment identifiers
function! s:CommentKeywordMatch(keyword)
  execute "syntax match swiftDocString \"\\v^\\s*-\\s*". a:keyword . "\\W\"hs=s+1,he=e-1 contained"
endfunction

syntax case ignore

call s:CommentKeywordMatch("attention")
call s:CommentKeywordMatch("author")
call s:CommentKeywordMatch("authors")
call s:CommentKeywordMatch("bug")
call s:CommentKeywordMatch("complexity")
call s:CommentKeywordMatch("copyright")
call s:CommentKeywordMatch("date")
call s:CommentKeywordMatch("experiment")
call s:CommentKeywordMatch("important")
call s:CommentKeywordMatch("invariant")
call s:CommentKeywordMatch("note")
call s:CommentKeywordMatch("parameter")
call s:CommentKeywordMatch("postcondition")
call s:CommentKeywordMatch("precondition")
call s:CommentKeywordMatch("remark")
call s:CommentKeywordMatch("remarks")
call s:CommentKeywordMatch("requires")
call s:CommentKeywordMatch("returns")
call s:CommentKeywordMatch("see")
call s:CommentKeywordMatch("since")
call s:CommentKeywordMatch("throws")
call s:CommentKeywordMatch("todo")
call s:CommentKeywordMatch("version")
call s:CommentKeywordMatch("warning")

syntax case match
delfunction s:CommentKeywordMatch


" Literals
" Strings
syntax region swiftString start=/"/ skip=/\\\\\|\\"/ end=/"/ contains=swiftInterpolatedWrapper oneline
syntax region swiftInterpolatedWrapper start="\v[^\\]\zs\\\(\s*" end="\v\s*\)" contained containedin=swiftString contains=swiftInterpolatedString,swiftString oneline
syntax match swiftInterpolatedString "\v\w+(\(\))?" contained containedin=swiftInterpolatedWrapper oneline

" Numbers
syntax match swiftNumber "\v<\d+>"
syntax match swiftNumber "\v<(\d+_+)+\d+(\.\d+(_+\d+)*)?>"
syntax match swiftNumber "\v<\d+\.\d+>"
syntax match swiftNumber "\v<\d*\.?\d+([Ee]-?)?\d+>"
syntax match swiftNumber "\v<0x[[:xdigit:]_]+([Pp]-?)?\x+>"
syntax match swiftNumber "\v<0b[01_]+>"
syntax match swiftNumber "\v<0o[0-7_]+>"

" BOOLs
syntax keyword swiftBoolean
      \ true
      \ false


" Operators
syntax match swiftOperator "\v\~"
syntax match swiftOperator "\v\s+!"
syntax match swiftOperator "\v\%"
syntax match swiftOperator "\v\^"
syntax match swiftOperator "\v\&"
syntax match swiftOperator "\v\*"
syntax match swiftOperator "\v-"
syntax match swiftOperator "\v\+"
syntax match swiftOperator "\v\="
syntax match swiftOperator "\v\|"
syntax match swiftOperator "\v\/"
syntax match swiftOperator "\v\."
syntax match swiftOperator "\v\<"
syntax match swiftOperator "\v\>"
syntax match swiftOperator "\v\?\?"

" Methods/Functions/Properties
syntax match swiftMethod "\(\.\)\@<=\w\+\((\)\@="
syntax match swiftProperty "\(\.\)\@<=\<\w\+\>(\@!"

" Swift closure arguments
syntax match swiftClosureArgument "\$\d\+\(\.\d\+\)\?"

syntax match swiftAvailability "\v((\*(\s*,\s*[a-zA-Z="0-9.]+)*)|(\w+\s+\d+(\.\d+(.\d+)?)?\s*,\s*)+\*)" contains=swiftString
syntax keyword swiftPlatforms OSX iOS watchOS OSXApplicationExtension iOSApplicationExtension contained containedin=swiftAvailability
syntax keyword swiftAvailabilityArg renamed unavailable introduced deprecated obsoleted message contained containedin=swiftAvailability

" Keywords {{{
syntax keyword swiftKeywords
      \ associatedtype
      \ associativity
      \ atexit
      \ break
      \ case
      \ catch
      \ class
      \ continue
      \ convenience
      \ default
      \ defer
      \ deinit
      \ didSet
      \ do
      \ dynamic
      \ else
      \ extension
      \ fallthrough
      \ fileprivate
      \ final
      \ for
      \ func
      \ get
      \ guard
      \ if
      \ import
      \ in
      \ infix
      \ init
      \ inout
      \ internal
      \ lazy
      \ let
      \ mutating
      \ nil
      \ nonmutating
      \ operator
      \ optional
      \ override
      \ postfix
      \ precedence
      \ precedencegroup
      \ prefix
      \ private
      \ protocol
      \ public
      \ repeat
      \ required
      \ return
      \ self
      \ set
      \ static
      \ subscript
      \ super
      \ switch
      \ throw
      \ try
      \ typealias
      \ unowned
      \ var
      \ weak
      \ where
      \ while
      \ willSet

syntax keyword swiftDefinitionModifier
      \ rethrows
      \ throws

syntax match swiftMultiwordKeywords "indirect case"
syntax match swiftMultiwordKeywords "indirect enum"
" }}}

" Names surrounded by backticks. This aren't limited to keywords because 1)
" Swift doesn't limit them to keywords and 2) I couldn't make the keywords not
" highlight at the same time
syntax region swiftEscapedReservedWord start="`" end="`" oneline

syntax keyword swiftAttributes
      \ @assignment
      \ @autoclosure
      \ @available
      \ @convention
      \ @discardableResult
      \ @exported
      \ @IBAction
      \ @IBDesignable
      \ @IBInspectable
      \ @IBOutlet
      \ @noescape
      \ @nonobjc
      \ @noreturn
      \ @NSApplicationMain
      \ @NSCopying
      \ @NSManaged
      \ @objc
      \ @testable
      \ @UIApplicationMain
      \ @warn_unused_result

syntax keyword swiftConditionStatement #available

syntax keyword swiftStructure
      \ struct
      \ enum

syntax keyword swiftDebugIdentifier
      \ #column
      \ #file
      \ #function
      \ #line
      \ __COLUMN__
      \ __FILE__
      \ __FUNCTION__
      \ __LINE__

syntax keyword swiftLineDirective #setline

syntax region swiftTypeWrapper start="\v:\s*" skip="\s*,\s*$*\s*" end="$\|/"me=e-1 contains=ALLBUT,swiftInterpolatedWrapper transparent
syntax region swiftTypeCastWrapper start="\(as\|is\)\(!\|?\)\=\s\+" end="\v(\s|$|\{)" contains=swiftType,swiftCastKeyword keepend transparent oneline
syntax region swiftGenericsWrapper start="\v\<" end="\v\>" contains=swiftType transparent oneline
syntax region swiftLiteralWrapper start="\v\=\s*" skip="\v[^\[\]]\(\)" end="\v(\[\]|\(\))" contains=ALL transparent oneline
syntax region swiftReturnWrapper start="\v-\>\s*" end="\v(\{|$)" contains=swiftType transparent oneline
syntax match swiftType "\v<\u\w*" contained containedin=swiftTypeWrapper,swiftLiteralWrapper,swiftGenericsWrapper,swiftTypeCastWrapper
syntax match swiftTypeDeclaration /->/ skipwhite nextgroup=swiftType

syntax keyword swiftImports import
syntax keyword swiftCastKeyword is as contained

" 'preprocesor' stuff
syntax keyword swiftPreprocessor
      \ #if
      \ #elseif
      \ #else
      \ #endif
      \ #selector


" Comment patterns
syntax match swiftComment "\v\/\/.*$"
      \ contains=swiftTodos,swiftDocString,swiftMarker,@Spell oneline
syntax region swiftComment start="/\*" end="\*/"
      \ contains=swiftTodos,swiftDocString,swiftMarker,@Spell fold


" Set highlights
highlight default link swiftTodos Todo
highlight default link swiftDocString String
highlight default link swiftShebang Comment
highlight default link swiftComment Comment
highlight default link swiftMarker Comment

highlight default link swiftString String
highlight default link swiftInterpolatedWrapper Delimiter
highlight default link swiftTypeDeclaration Delimiter
highlight default link swiftNumber Number
highlight default link swiftBoolean Boolean

highlight default link swiftOperator Operator
highlight default link swiftCastKeyword Keyword
highlight default link swiftKeywords Keyword
highlight default link swiftMultiwordKeywords Keyword
highlight default link swiftEscapedReservedWord Normal
highlight default link swiftClosureArgument Operator
highlight default link swiftAttributes PreProc
highlight default link swiftConditionStatement PreProc
highlight default link swiftStructure Structure
highlight default link swiftType Type
highlight default link swiftImports Include
highlight default link swiftPreprocessor PreProc
highlight default link swiftMethod Function
highlight default link swiftProperty Identifier

highlight default link swiftDefinitionModifier Define
highlight default link swiftConditionStatement PreProc
highlight default link swiftAvailability Normal
highlight default link swiftAvailabilityArg Normal
highlight default link swiftPlatforms Keyword
highlight default link swiftDebugIdentifier PreProc
highlight default link swiftLineDirective PreProc

" Force vim to sync at least x lines. This solves the multiline comment not
" being highlighted issue
syn sync minlines=100

let b:current_syntax = "swift"
