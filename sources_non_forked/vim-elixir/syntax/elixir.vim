if exists("b:current_syntax")
  finish
endif

" syncing starts 2000 lines before top line so docstrings don't screw things up
syn sync minlines=2000

syn cluster elixirNotTop contains=@elixirRegexSpecial,@elixirStringContained,@elixirDeclaration,elixirTodo,elixirArguments,elixirBlockDefinition

syn match elixirComment '#.*' contains=elixirTodo,@Spell
syn keyword elixirTodo FIXME NOTE TODO OPTIMIZE XXX HACK contained

syn keyword elixirKeyword case when with cond for if unless try receive send
syn keyword elixirKeyword exit raise throw after rescue catch else
syn keyword elixirKeyword quote unquote super spawn spawn_link spawn_monitor

" Functions used on guards
syn keyword elixirKeyword contained is_atom is_binary is_bitstring is_boolean
syn keyword elixirKeyword contained is_float is_function is_integer is_list
syn keyword elixirKeyword contained is_map is_nil is_number is_pid is_port
syn keyword elixirKeyword contained is_record is_reference is_tuple is_exception
syn keyword elixirKeyword contained abs bit_size byte_size div elem hd length
syn keyword elixirKeyword contained map_size node rem round tl trunc tuple_size

syn match elixirGuard '.*when.*' contains=ALLBUT,@elixirNotTop

syn keyword elixirInclude import require alias use

syn keyword elixirSelf self

syn match elixirId '\<[_a-zA-Z]\w*[!?]\?\>'

" This unfortunately also matches function names in function calls
syn match elixirUnusedVariable '\(([^)]*\)\@<=\<_\w*\>'

syn keyword elixirOperator and not or when xor in
syn match   elixirOperator '!==\|!=\|!'
syn match   elixirOperator '=\~\|===\|==\|='
syn match   elixirOperator '<<<\|<<\|<=\|<-\|<'
syn match   elixirOperator '>>>\|>>\|>=\|>'
syn match   elixirOperator '->\|--\|-'
syn match   elixirOperator '++\|+'
syn match   elixirOperator '&&&\|&&\|&'
syn match   elixirOperator '|||\|||\||>\||'
syn match   elixirOperator '\.\.\|\.'
syn match   elixirOperator "\^\^\^\|\^"
syn match   elixirOperator '\\\\\|::\|\*\|/\|\~\~\~\|@'

syn match   elixirAtom '\(:\)\@<!:\%([a-zA-Z_]\w*\%([?!]\|=[>=]\@!\)\?\|<>\|===\?\|>=\?\|<=\?\)'
syn match   elixirAtom '\(:\)\@<!:\%(<=>\|&&\?\|%\(()\|\[\]\|{}\)\|++\?\|--\?\|||\?\|!\|//\|[%&`/|]\)'
syn match   elixirAtom "\%([a-zA-Z_]\w*[?!]\?\):\(:\)\@!"

syn match   elixirAlias '\<[!]\?[A-Z]\w*\(\.[A-Z]\w*\)*\>'

syn keyword elixirBoolean true false nil

syn match elixirVariable '@[a-z]\w*'
syn match elixirVariable '&\d\+'

syn keyword elixirPseudoVariable __FILE__ __DIR__ __MODULE__ __ENV__ __CALLER__

syn match elixirNumber '\<\d\(_\?\d\)*\(\.[^[:space:][:digit:]]\@!\(_\?\d\)*\)\?\([eE][-+]\?\d\(_\?\d\)*\)\?\>'
syn match elixirNumber '\<0[xX][0-9A-Fa-f]\+\>'
syn match elixirNumber '\<0[bB][01]\+\>'

syn match elixirRegexEscape            "\\\\\|\\[aAbBcdDefGhHnrsStvVwW]\|\\\d\{3}\|\\x[0-9a-fA-F]\{2}" contained
syn match elixirRegexEscapePunctuation "?\|\\.\|*\|\\\[\|\\\]\|+\|\\^\|\\\$\|\\|\|\\(\|\\)\|\\{\|\\}" contained
syn match elixirRegexQuantifier        "[*?+][?+]\=" contained display
syn match elixirRegexQuantifier        "{\d\+\%(,\d*\)\=}?\=" contained display
syn match elixirRegexCharClass         "\[:\(alnum\|alpha\|ascii\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|word\|xdigit\):\]" contained display

syn region elixirRegex matchgroup=elixirRegexDelimiter start="%r/" end="/[uiomxfr]*" skip="\\\\" contains=@elixirRegexSpecial

syn cluster elixirRegexSpecial    contains=elixirRegexEscape,elixirRegexCharClass,elixirRegexQuantifier,elixirRegexEscapePunctuation
syn cluster elixirStringContained contains=elixirInterpolation,elixirRegexEscape,elixirRegexCharClass

syn region elixirString        matchgroup=elixirStringDelimiter start="'" end="'" skip="\\'\|\\\\"
syn region elixirString        matchgroup=elixirStringDelimiter start='"' end='"' skip='\\"' contains=@elixirStringContained
syn region elixirInterpolation matchgroup=elixirInterpolationDelimiter start="#{" end="}" contained contains=ALLBUT,elixirComment,@elixirNotTop

syn region elixirDocStringStart matchgroup=elixirDocString start=+"""+ end=+$+ oneline contains=ALLBUT,@elixirNotTop
syn region elixirDocStringStart matchgroup=elixirDocString start=+'''+ end=+$+ oneline contains=ALLBUT,@elixirNotTop
syn region elixirDocString     start=+\z("""\)+ end=+^\s*\zs\z1+ contains=elixirDocStringStart,elixirTodo,elixirInterpolation,@Spell keepend fold
syn region elixirDocString     start=+\z('''\)+ end=+^\s*\zs\z1+ contains=elixirDocStringStart,elixirTodo,elixirInterpolation,@Spell keepend fold

syn match elixirAtomInterpolated   ':\("\)\@=' contains=elixirString
syn match elixirString             "\(\w\)\@<!?\%(\\\(x\d{1,2}\|\h{1,2}\h\@!\>\|0[0-7]{0,2}[0-7]\@!\>\|[^x0MC]\)\|(\\[MC]-)+\w\|[^\s\\]\)"

syn region elixirBlock              matchgroup=elixirBlockDefinition start="\<do\>:\@!" end="\<end\>" contains=ALLBUT,@elixirNotTop fold
syn region elixirAnonymousFunction  matchgroup=elixirBlockDefinition start="\<fn\>"         end="\<end\>" contains=ALLBUT,@elixirNotTop fold

syn region elixirArguments start="(" end=")" contained contains=elixirOperator,elixirAtom,elixirPseudoVariable,elixirAlias,elixirBoolean,elixirVariable,elixirUnusedVariable,elixirNumber,elixirDocString,elixirAtomInterpolated,elixirRegex,elixirString,elixirStringDelimiter,elixirRegexDelimiter,elixirInterpolationDelimiter,elixirSigilDelimiter

syn match elixirDelimEscape "\\[(<{\[)>}\]/\"'|]" transparent display contained contains=NONE

syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\u\z(/\|\"\|'\||\)" end="\z1" skip="\\\\\|\\\z1" contains=elixirDelimEscape fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\u{"                end="}"   skip="\\\\\|\\}"   contains=elixirDelimEscape fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\u<"                end=">"   skip="\\\\\|\\>"   contains=elixirDelimEscape fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\u\["               end="\]"  skip="\\\\\|\\\]"  contains=elixirDelimEscape fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\u("                end=")"   skip="\\\\\|\\)"   contains=elixirDelimEscape fold

syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l\z(/\|\"\|'\||\)" end="\z1" skip="\\\\\|\\\z1"                                                              fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l{"                end="}"   skip="\\\\\|\\}"   contains=@elixirStringContained,elixirRegexEscapePunctuation fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l<"                end=">"   skip="\\\\\|\\>"   contains=@elixirStringContained,elixirRegexEscapePunctuation fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l\["               end="\]"  skip="\\\\\|\\\]"  contains=@elixirStringContained,elixirRegexEscapePunctuation fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l("                end=")"   skip="\\\\\|\\)"   contains=@elixirStringContained,elixirRegexEscapePunctuation fold

" Sigils surrounded with docString
syn region elixirSigil matchgroup=elixirSigilDelimiter start=+\~\a\z("""\)+ end=+^\s*\zs\z1+ skip=+\\"+ fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start=+\~\a\z('''\)+ end=+^\s*\zs\z1+ skip=+\\'+ fold

" Defines
syn keyword elixirDefine              def            nextgroup=elixirFunctionDeclaration    skipwhite skipnl
syn keyword elixirPrivateDefine       defp           nextgroup=elixirFunctionDeclaration    skipwhite skipnl
syn keyword elixirModuleDefine        defmodule      nextgroup=elixirModuleDeclaration      skipwhite skipnl
syn keyword elixirProtocolDefine      defprotocol    nextgroup=elixirProtocolDeclaration    skipwhite skipnl
syn keyword elixirImplDefine          defimpl        nextgroup=elixirImplDeclaration        skipwhite skipnl
syn keyword elixirRecordDefine        defrecord      nextgroup=elixirRecordDeclaration      skipwhite skipnl
syn keyword elixirPrivateRecordDefine defrecordp     nextgroup=elixirRecordDeclaration      skipwhite skipnl
syn keyword elixirMacroDefine         defmacro       nextgroup=elixirMacroDeclaration       skipwhite skipnl
syn keyword elixirPrivateMacroDefine  defmacrop      nextgroup=elixirMacroDeclaration       skipwhite skipnl
syn keyword elixirDelegateDefine      defdelegate    nextgroup=elixirDelegateDeclaration    skipwhite skipnl
syn keyword elixirOverridableDefine   defoverridable nextgroup=elixirOverridableDeclaration skipwhite skipnl
syn keyword elixirExceptionDefine     defexception   nextgroup=elixirExceptionDeclaration   skipwhite skipnl
syn keyword elixirCallbackDefine      defcallback    nextgroup=elixirCallbackDeclaration    skipwhite skipnl
syn keyword elixirStructDefine        defstruct      skipwhite skipnl

" Declarations
syn match  elixirModuleDeclaration      "[^[:space:];#<]\+"        contained contains=elixirAlias nextgroup=elixirBlock     skipwhite skipnl
syn match  elixirFunctionDeclaration    "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirProtocolDeclaration    "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirImplDeclaration        "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirRecordDeclaration      "[^[:space:];#<]\+"        contained contains=elixirAlias,elixirAtom                skipwhite skipnl
syn match  elixirMacroDeclaration       "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirDelegateDeclaration    "[^[:space:];#<,()\[\]]\+" contained contains=elixirFunctionDeclaration             skipwhite skipnl
syn region elixirDelegateDeclaration    start='\['     end='\]'    contained contains=elixirFunctionDeclaration             skipwhite skipnl
syn match  elixirOverridableDeclaration "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirExceptionDeclaration   "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirCallbackDeclaration    "[^[:space:];#<,()\[\]]\+" contained contains=elixirFunctionDeclaration             skipwhite skipnl

syn cluster elixirDeclaration contains=elixirFunctionDeclaration,elixirModuleDeclaration,elixirProtocolDeclaration,elixirImplDeclaration,elixirRecordDeclaration,elixirMacroDeclaration,elixirDelegateDeclaration,elixirOverridableDeclaration,elixirExceptionDeclaration,elixirCallbackDeclaration,elixirStructDeclaration

hi def link elixirBlockDefinition        Keyword
hi def link elixirDefine                 Define
hi def link elixirPrivateDefine          Define
hi def link elixirModuleDefine           Define
hi def link elixirProtocolDefine         Define
hi def link elixirImplDefine             Define
hi def link elixirRecordDefine           Define
hi def link elixirPrivateRecordDefine    Define
hi def link elixirMacroDefine            Define
hi def link elixirPrivateMacroDefine     Define
hi def link elixirDelegateDefine         Define
hi def link elixirOverridableDefine      Define
hi def link elixirExceptionDefine        Define
hi def link elixirCallbackDefine         Define
hi def link elixirStructDefine           Define
hi def link elixirFunctionDeclaration    Function
hi def link elixirMacroDeclaration       Macro
hi def link elixirInclude                Include
hi def link elixirComment                Comment
hi def link elixirTodo                   Todo
hi def link elixirKeyword                Keyword
hi def link elixirOperator               Operator
hi def link elixirAtom                   Constant
hi def link elixirPseudoVariable         Constant
hi def link elixirAlias                  Type
hi def link elixirBoolean                Boolean
hi def link elixirVariable               Identifier
hi def link elixirSelf                   Identifier
hi def link elixirUnusedVariable         Comment
hi def link elixirNumber                 Number
hi def link elixirDocString              String
hi def link elixirAtomInterpolated       elixirAtom
hi def link elixirRegex                  elixirString
hi def link elixirRegexEscape            elixirSpecial
hi def link elixirRegexEscapePunctuation elixirSpecial
hi def link elixirRegexCharClass         elixirSpecial
hi def link elixirRegexQuantifier        elixirSpecial
hi def link elixirSpecial                Special
hi def link elixirString                 String
hi def link elixirSigil                  String
hi def link elixirStringDelimiter        Delimiter
hi def link elixirRegexDelimiter         Delimiter
hi def link elixirInterpolationDelimiter Delimiter
hi def link elixirSigilDelimiter         Delimiter

let b:current_syntax = "elixir"
