if !exists("main_syntax")
  if exists("b:current_syntax")
    finish
  endif
  let main_syntax = "elixir"
endif

let s:cpo_save = &cpo
set cpo&vim

syn cluster elixirNotTop contains=@elixirRegexSpecial,@elixirStringContained,@elixirDeclaration,elixirTodo,elixirArguments,elixirBlockDefinition,elixirUnusedVariable,elixirStructDelimiter,elixirListDelimiter
syn cluster elixirRegexSpecial contains=elixirRegexEscape,elixirRegexCharClass,elixirRegexQuantifier,elixirRegexEscapePunctuation
syn cluster elixirStringContained contains=elixirInterpolation,elixirRegexEscape,elixirRegexCharClass
syn cluster elixirDeclaration contains=elixirFunctionDeclaration,elixirPrivateFunctionDeclaration,elixirModuleDeclaration,elixirProtocolDeclaration,elixirImplDeclaration,elixirRecordDeclaration,elixirPrivateRecordDeclaration,elixirMacroDeclaration,elixirPrivateMacroDeclaration,elixirDelegateDeclaration,elixirOverridableDeclaration,elixirExceptionDeclaration,elixirCallbackDeclaration,elixirStructDeclaration

syn match elixirComment '#.*' contains=elixirTodo,@Spell
syn keyword elixirTodo FIXME NOTE TODO OPTIMIZE XXX HACK contained

syn match elixirId '\<[_a-zA-Z]\w*[!?]\?\>' contains=elixirUnusedVariable

syn match elixirKeyword '\(\.\)\@<!\<\(for\|case\|when\|with\|cond\|if\|unless\|try\|receive\|after\|raise\|rescue\|catch\|else\|quote\|unquote\|super\|unquote_splicing\)\>:\@!'

syn keyword elixirInclude import require alias use

syn keyword elixirSelf self

syn match elixirUnusedVariable contained '\%(\.\)\@<!\<_\w*\>\%((\)\@!'

syn match   elixirOperator '\v\.@<!<%(and|or|in|not)>'
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
syn match   elixirOperator '\~>\|\~>>\|<\~\|<<\~\|<\~>'

syn match   elixirAlias '\([a-z]\)\@<![A-Z]\w*\%(\.[A-Z]\w*\)*'

syn match   elixirAtom '\(:\)\@<!:\%([a-zA-Z_*]\w*\%([?!]\|=[>=]\@!\)\?\|<>\|===\?\|>=\?\|<=\?\)'
syn match   elixirAtom '\(:\)\@<!:\%(<=>\|&&\?\|%\(()\|\[\]\|{}\)\|++\?\|--\?\|||\?\|!\|//\|[%&`/|]\)'
syn match   elixirAtom "\%([a-zA-Z_]\w*[?!]\?\):\(:\)\@!"

syn keyword elixirBoolean true false nil

syn match elixirVariable '@[a-z]\w*'
syn match elixirVariable '&\d\+'

syn keyword elixirPseudoVariable __FILE__ __DIR__ __MODULE__ __ENV__ __CALLER__ __STACKTRACE__

syn match elixirNumber '\<-\?\d\(_\?\d\)*\(\.[^[:space:][:digit:]]\@!\(_\?\d\)*\)\?\([eE][-+]\?\d\(_\?\d\)*\)\?\>'
syn match elixirNumber '\<-\?0[xX][0-9A-Fa-f]\+\>'
syn match elixirNumber '\<-\?0[oO][0-7]\+\>'
syn match elixirNumber '\<-\?0[bB][01]\+\>'

syn match elixirRegexEscape            "\\\\\|\\[aAbBcdDefGhHnrsStvVwW]\|\\\d\{3}\|\\x[0-9a-fA-F]\{2}" contained
syn match elixirRegexEscapePunctuation "?\|\\.\|*\|\\\[\|\\\]\|+\|\\^\|\\\$\|\\|\|\\(\|\\)\|\\{\|\\}" contained
syn match elixirRegexQuantifier        "[*?+][?+]\=" contained display
syn match elixirRegexQuantifier        "{\d\+\%(,\d*\)\=}?\=" contained display
syn match elixirRegexCharClass         "\[:\(alnum\|alpha\|ascii\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|word\|xdigit\):\]" contained display

syn region elixirRegex matchgroup=elixirRegexDelimiter start="%r/" end="/[uiomxfr]*" skip="\\\\" contains=@elixirRegexSpecial

syn region elixirTuple matchgroup=elixirTupleDelimiter start="\(\w\|#\)\@<!{" end="}" contains=ALLBUT,@elixirNotTop fold

syn match elixirListDelimiter '\[' contained containedin=elixirList
syn region elixirList matchgroup=elixirListDelimiter start='\[' end='\]' contains=ALLBUT,@elixirNotTop fold

syn match elixirStructDelimiter '{' contained containedin=elixirStruct
syn region elixirStruct matchgroup=elixirStructDelimiter start="%\(\w\+{\)\@=" end="}" contains=ALLBUT,@elixirNotTop fold

syn region elixirMap matchgroup=elixirMapDelimiter start="%{" end="}" contains=ALLBUT,@elixirNotTop fold

syn region elixirCharList  matchgroup=elixirCharListDelimiter start=+\z('\)+   end=+\z1+ skip=+\\\\\|\\\z1+  contains=@Spell,@elixirStringContained
syn region elixirString  matchgroup=elixirStringDelimiter start=+\z("\)+   end=+\z1+ skip=+\\\\\|\\\z1+  contains=@Spell,@elixirStringContained
syn region elixirString  matchgroup=elixirStringDelimiter start=+\z('''\)+ end=+^\s*\z1+ contains=@Spell,@elixirStringContained
syn region elixirString  matchgroup=elixirStringDelimiter start=+\z("""\)+ end=+^\s*\z1+ contains=@Spell,@elixirStringContained
syn region elixirInterpolation matchgroup=elixirInterpolationDelimiter start="#{" end="}" contained contains=ALLBUT,elixirComment,@elixirNotTop

syn match elixirAtomInterpolated   ':\("\)\@=' contains=elixirString
syn match elixirString             "\(\w\)\@<!?\%(\\\(x\d{1,2}\|\h{1,2}\h\@!\>\|0[0-7]{0,2}[0-7]\@!\>\|[^x0MC]\)\|(\\[MC]-)+\w\|[^\s\\]\)"

syn region elixirBlock              matchgroup=elixirBlockDefinition start="\<do\>:\@!" end="\<end\>" contains=ALLBUT,@elixirNotTop fold
syn region elixirAnonymousFunction  matchgroup=elixirBlockDefinition start="\<fn\>"     end="\<end\>" contains=ALLBUT,@elixirNotTop fold

syn region elixirArguments start="(" end=")" contained contains=elixirOperator,elixirAtom,elixirPseudoVariable,elixirAlias,elixirBoolean,elixirVariable,elixirUnusedVariable,elixirNumber,elixirDocString,elixirAtomInterpolated,elixirRegex,elixirString,elixirStringDelimiter,elixirRegexDelimiter,elixirInterpolationDelimiter,elixirSigil,elixirAnonymousFunction,elixirComment,elixirCharList,elixirCharListDelimiter

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
syn region elixirSigil matchgroup=elixirSigilDelimiter start="\~\l\/"               end="\/"  skip="\\\\\|\\\/"  contains=@elixirStringContained,elixirRegexEscapePunctuation fold

" Sigils surrounded with heredoc
syn region elixirSigil matchgroup=elixirSigilDelimiter start=+\~\a\z("""\)+ end=+^\s*\z1+ skip=+\\"+ fold
syn region elixirSigil matchgroup=elixirSigilDelimiter start=+\~\a\z('''\)+ end=+^\s*\z1+ skip=+\\'+ fold


" LiveView Sigils surrounded with ~L"""
syntax include @HTML syntax/html.vim
unlet b:current_syntax
syntax region elixirLiveViewSigil matchgroup=elixirSigilDelimiter keepend start=+\~L\z("""\)+ end=+^\s*\z1+ skip=+\\"+ contains=@HTML fold
syntax region elixirSurfaceSigil matchgroup=elixirSigilDelimiter keepend start=+\~H\z("""\)+ end=+^\s*\z1+ skip=+\\"+ contains=@HTML fold
syntax region elixirSurfaceSigil matchgroup=elixirSigilDelimiter keepend start=+\~F\z("""\)+ end=+^\s*\z1+ skip=+\\"+ contains=@HTML fold
syntax region elixirPhoenixESigil matchgroup=elixirSigilDelimiter keepend start=+\~E\z("""\)+ end=+^\s*\z1+ skip=+\\"+ contains=@HTML fold
syntax region elixirPhoenixeSigil matchgroup=elixirSigilDelimiter keepend start=+\~e\z("""\)+ end=+^\s*\z1+ skip=+\\"+ contains=@HTML fold

" Documentation
if exists('g:elixir_use_markdown_for_docs') && g:elixir_use_markdown_for_docs
  syn include @markdown syntax/markdown.vim
  unlet b:current_syntax
  syn cluster elixirDocStringContained contains=@markdown,@Spell,elixirInterpolation
else
  let g:elixir_use_markdown_for_docs = 0
  syn cluster elixirDocStringContained contains=elixirDocTest,elixirTodo,@Spell,elixirInterpolation

  " doctests
  syn region elixirDocTest start="^\s*\%(iex\|\.\.\.\)\%((\d*)\)\?>\s" end="^\s*$" contained
endif

syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start="\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]\z(/\|\"\|'\||\)" end="\z1" skip="\\\\\|\\\z1" contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start="\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]{"                end="}"   skip="\\\\\|\\}"   contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start="\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]<"                end=">"   skip="\\\\\|\\>"   contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start="\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]\["               end="\]"  skip="\\\\\|\\\]"  contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start="\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]("                end=")"   skip="\\\\\|\\)"   contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocStringDelimiter start=+\%(@\w*doc\(\s\|(\)\+\)\@<=\z("\)+                 end=+\z1+ skip=+\\\\\|\\\z1+ contains=@elixirDocStringContained      keepend
syn region elixirDocString matchgroup=elixirDocStringDelimiter start=+\%(@\w*doc\(\s\|(\)\+\)\@<=\z("""\)+               end=+^\s*\z1+                contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start=+\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]\z('''\)+         end=+^\s*\z1+                contains=@elixirDocStringContained fold keepend
syn region elixirDocString matchgroup=elixirDocSigilDelimiter  start=+\%(@\w*doc\(\s\|(\)\+\)\@<=\~[Ss]\z("""\)+         end=+^\s*\z1+                contains=@elixirDocStringContained fold keepend

" Defines
syn match elixirDefine              '\<def\>\(:\)\@!'             nextgroup=elixirFunctionDeclaration        skipwhite skipnl
syn match elixirPrivateDefine       '\<defp\>\(:\)\@!'            nextgroup=elixirPrivateFunctionDeclaration skipwhite skipnl
syn match elixirNumericalDefine     '\<defn\>\(:\)\@!'            nextgroup=elixirFunctionDeclaration        skipwhite skipnl
syn match elixirGuard               '\<defguard\>\(:\)\@!'        nextgroup=elixirFunctionDeclaration        skipwhite skipnl
syn match elixirPrivateGuard        '\<defguardp\>\(:\)\@!'       nextgroup=elixirPrivateFunctionDeclaration skipwhite skipnl
syn match elixirModuleDefine        '\<defmodule\>\(:\)\@!'       nextgroup=elixirModuleDeclaration          skipwhite skipnl
syn match elixirProtocolDefine      '\<defprotocol\>\(:\)\@!'     nextgroup=elixirProtocolDeclaration        skipwhite skipnl
syn match elixirImplDefine          '\<defimpl\>\(:\)\@!'         nextgroup=elixirImplDeclaration            skipwhite skipnl
syn match elixirRecordDefine        '\<defrecord\>\(:\)\@!'       nextgroup=elixirRecordDeclaration          skipwhite skipnl
syn match elixirPrivateRecordDefine '\<defrecordp\>\(:\)\@!'      nextgroup=elixirPrivateRecordDeclaration   skipwhite skipnl
syn match elixirMacroDefine         '\<defmacro\>\(:\)\@!'        nextgroup=elixirMacroDeclaration           skipwhite skipnl
syn match elixirPrivateMacroDefine  '\<defmacrop\>\(:\)\@!'       nextgroup=elixirPrivateMacroDeclaration    skipwhite skipnl
syn match elixirDelegateDefine      '\<defdelegate\>\(:\)\@!'     nextgroup=elixirDelegateDeclaration        skipwhite skipnl
syn match elixirOverridableDefine   '\<defoverridable\>\(:\)\@!'  nextgroup=elixirOverridableDeclaration     skipwhite skipnl
syn match elixirExceptionDefine     '\<defexception\>\(:\)\@!'    nextgroup=elixirExceptionDeclaration       skipwhite skipnl
syn match elixirCallbackDefine      '\<defcallback\>\(:\)\@!'     nextgroup=elixirCallbackDeclaration        skipwhite skipnl
syn match elixirStructDefine        '\<defstruct\>\(:\)\@!'       skipwhite skipnl

" Declarations
syn match  elixirModuleDeclaration          "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirBlock     skipwhite skipnl
syn match  elixirFunctionDeclaration        "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirPrivateFunctionDeclaration "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirProtocolDeclaration        "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirImplDeclaration            "[^[:space:];#<]\+"        contained contains=elixirAlias                           skipwhite skipnl
syn match  elixirRecordDeclaration          "[^[:space:];#<]\+"        contained contains=elixirAlias,elixirAtom                skipwhite skipnl
syn match  elixirPrivateRecordDeclaration   "[^[:space:];#<]\+"        contained contains=elixirAlias,elixirAtom                skipwhite skipnl
syn match  elixirMacroDeclaration           "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirPrivateMacroDeclaration    "[^[:space:];#<,()\[\]]\+" contained                      nextgroup=elixirArguments skipwhite skipnl
syn match  elixirDelegateDeclaration        "[^[:space:];#<,()\[\]]\+" contained contains=elixirFunctionDeclaration             skipwhite skipnl
syn region elixirDelegateDeclaration        start='\['     end='\]'    contained contains=elixirFunctionDeclaration             skipwhite skipnl
syn match  elixirOverridableDeclaration     "[^[:space:];#<]\+"        contained contains=elixirAlias,elixirAtom                skipwhite skipnl
syn match  elixirExceptionDeclaration       "[^[:space:];#<]\+"        contained contains=elixirAlias,elixirAtom                skipwhite skipnl
syn match  elixirCallbackDeclaration        "[^[:space:];#<,()\[\]]\+" contained contains=elixirFunctionDeclaration             skipwhite skipnl

" ExUnit
syn match  elixirExUnitMacro "\C\(^\s*\)\@<=\<\(test\|describe\|setup\|setup_all\|on_exit\|doctest\)\>"
syn match  elixirExUnitAssert "\C\(^\s*\)\@<=\<\(assert\|assert_in_delta\|assert_raise\|assert_receive\|assert_received\|catch_error\)\>"
syn match  elixirExUnitAssert "\C\(^\s*\)\@<=\<\(catch_exit\|catch_throw\|flunk\|refute\|refute_in_delta\|refute_receive\|refute_received\)\>"

" syncing starts 2000 lines before top line so docstrings don't screw things up
syn sync minlines=2000

hi def link elixirBlockDefinition            Define
hi def link elixirDefine                     Define
hi def link elixirPrivateDefine              Define
hi def link elixirNumericalDefine            Define
hi def link elixirGuard                      Define
hi def link elixirPrivateGuard               Define
hi def link elixirModuleDefine               Define
hi def link elixirProtocolDefine             Define
hi def link elixirImplDefine                 Define
hi def link elixirRecordDefine               Define
hi def link elixirPrivateRecordDefine        Define
hi def link elixirMacroDefine                Define
hi def link elixirPrivateMacroDefine         Define
hi def link elixirDelegateDefine             Define
hi def link elixirOverridableDefine          Define
hi def link elixirExceptionDefine            Define
hi def link elixirCallbackDefine             Define
hi def link elixirStructDefine               Define
hi def link elixirExUnitMacro                Define
hi def link elixirModuleDeclaration          Type
hi def link elixirPrivateFunctionDeclaration elixirFunctionDeclaration
hi def link elixirFunctionDeclaration        Function
hi def link elixirPrivateMacroDeclaration    elixirMacroDeclaration
hi def link elixirMacroDeclaration           Macro
hi def link elixirInclude                    Include
hi def link elixirComment                    Comment
hi def link elixirTodo                       Todo
hi def link elixirKeyword                    Define
hi def link elixirExUnitAssert               Keyword
hi def link elixirOperator                   Operator
hi def link elixirAtom                       Constant
hi def link elixirPseudoVariable             Constant
hi def link elixirAlias                      Type
hi def link elixirBoolean                    Boolean
hi def link elixirVariable                   Identifier
hi def link elixirSelf                       Identifier
hi def link elixirUnusedVariable             Comment
hi def link elixirNumber                     Number
hi def link elixirDocString                  Comment
hi def link elixirDocTest                    elixirKeyword
hi def link elixirAtomInterpolated           elixirAtom
hi def link elixirRegex                      elixirString
hi def link elixirRegexEscape                elixirSpecial
hi def link elixirRegexEscapePunctuation     elixirSpecial
hi def link elixirRegexCharClass             elixirSpecial
hi def link elixirRegexQuantifier            elixirSpecial
hi def link elixirSpecial                    Special
hi def link elixirString                     String
hi def link elixirCharList                   String
hi def link elixirSigil                      String
hi def link elixirDocStringDelimiter         elixirStringDelimiter
hi def link elixirDocSigilDelimiter          elixirSigilDelimiter
hi def link elixirStringDelimiter            Delimiter
hi def link elixirCharListDelimiter          Delimiter
hi def link elixirRegexDelimiter             Delimiter
hi def link elixirInterpolationDelimiter     Delimiter
hi def link elixirSigilDelimiter             Delimiter
hi def link elixirPrivateRecordDeclaration   elixirRecordDeclaration

let b:current_syntax = "elixir"

if main_syntax == "elixir"
  unlet main_syntax
endif

let &cpo = s:cpo_save
unlet s:cpo_save
