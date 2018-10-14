" Copyright 2009 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" go.vim: Vim syntax file for Go.

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn case match

syn keyword     goPackage           package
syn keyword     goImport            import    contained
syn keyword     goVar               var       contained
syn keyword     goConst             const     contained

hi def link     goPackage           Statement
hi def link     goImport            Statement
hi def link     goVar               Keyword
hi def link     goConst             Keyword
hi def link     goDeclaration       Keyword

" Keywords within functions
syn keyword     goStatement         defer go goto return break continue fallthrough
syn keyword     goConditional       if else switch select
syn keyword     goLabel             case default
syn keyword     goRepeat            for range

hi def link     goStatement         Statement
hi def link     goConditional       Conditional
hi def link     goLabel             Label
hi def link     goRepeat            Repeat

" Predefined types
syn keyword     goType              chan map bool string error
syn keyword     goSignedInts        int int8 int16 int32 int64 rune
syn keyword     goUnsignedInts      byte uint uint8 uint16 uint32 uint64 uintptr
syn keyword     goFloats            float32 float64
syn keyword     goComplexes         complex64 complex128

hi def link     goType              Type
hi def link     goSignedInts        Type
hi def link     goUnsignedInts      Type
hi def link     goFloats            Type
hi def link     goComplexes         Type

" Predefined functions and values
syn keyword     goBuiltins                 append cap close complex copy delete imag len
syn keyword     goBuiltins                 make new panic print println real recover
syn keyword     goBoolean                  true false
syn keyword     goPredefinedIdentifiers    nil iota

hi def link     goBuiltins                 Identifier
hi def link     goBoolean                  Boolean
hi def link     goPredefinedIdentifiers    goBoolean

" Comments; their contents
syn keyword     goTodo              contained TODO FIXME XXX BUG
syn cluster     goCommentGroup      contains=goTodo

syn region      goComment           start="//" end="$" contains=goGenerate,@goCommentGroup,@Spell
if go#config#FoldEnable('comment')
  syn region    goComment           start="/\*" end="\*/" contains=@goCommentGroup,@Spell fold
  syn match     goComment           "\v(^\s*//.*\n)+" contains=goGenerate,@goCommentGroup,@Spell fold
else
  syn region    goComment           start="/\*" end="\*/" contains=@goCommentGroup,@Spell
endif

hi def link     goComment           Comment
hi def link     goTodo              Todo

if go#config#HighlightGenerateTags()
  syn match       goGenerateVariables contained /\(\$GOARCH\|\$GOOS\|\$GOFILE\|\$GOLINE\|\$GOPACKAGE\|\$DOLLAR\)\>/
  syn region      goGenerate          start="^\s*//go:generate" end="$" contains=goGenerateVariables
  hi def link     goGenerate          PreProc
  hi def link     goGenerateVariables Special
endif

" Go escapes
syn match       goEscapeOctal       display contained "\\[0-7]\{3}"
syn match       goEscapeC           display contained +\\[abfnrtv\\'"]+
syn match       goEscapeX           display contained "\\x\x\{2}"
syn match       goEscapeU           display contained "\\u\x\{4}"
syn match       goEscapeBigU        display contained "\\U\x\{8}"
syn match       goEscapeError       display contained +\\[^0-7xuUabfnrtv\\'"]+

hi def link     goEscapeOctal       goSpecialString
hi def link     goEscapeC           goSpecialString
hi def link     goEscapeX           goSpecialString
hi def link     goEscapeU           goSpecialString
hi def link     goEscapeBigU        goSpecialString
hi def link     goSpecialString     Special
hi def link     goEscapeError       Error

" Strings and their contents
syn cluster     goStringGroup       contains=goEscapeOctal,goEscapeC,goEscapeX,goEscapeU,goEscapeBigU,goEscapeError
if go#config#HighlightStringSpellcheck()
  syn region      goString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@goStringGroup,@Spell
  syn region      goRawString         start=+`+ end=+`+ contains=@Spell
else
  syn region      goString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@goStringGroup
  syn region      goRawString         start=+`+ end=+`+
endif

if go#config#HighlightFormatStrings()
  " [n] notation is valid for specifying explicit argument indexes
  " 1. Match a literal % not preceded by a %.
  " 2. Match any number of -, #, 0, space, or +
  " 3. Match * or [n]* or any number or nothing before a .
  " 4. Match * or [n]* or any number or nothing after a .
  " 5. Match [n] or nothing before a verb
  " 6. Match a formatting verb
  syn match       goFormatSpecifier   /\
        \([^%]\(%%\)*\)\
        \@<=%[-#0 +]*\
        \%(\%(\%(\[\d\+\]\)\=\*\)\|\d\+\)\=\
        \%(\.\%(\%(\%(\[\d\+\]\)\=\*\)\|\d\+\)\=\)\=\
        \%(\[\d\+\]\)\=[vTtbcdoqxXUeEfFgGsp]/ contained containedin=goString,goRawString
  hi def link     goFormatSpecifier   goSpecialString
endif

hi def link     goString            String
hi def link     goRawString         String

" Characters; their contents
syn cluster     goCharacterGroup    contains=goEscapeOctal,goEscapeC,goEscapeX,goEscapeU,goEscapeBigU
syn region      goCharacter         start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=@goCharacterGroup

hi def link     goCharacter         Character

" Regions
syn region      goParen             start='(' end=')' transparent
if go#config#FoldEnable('block')
  syn region    goBlock             start="{" end="}" transparent fold
else
  syn region    goBlock             start="{" end="}" transparent
endif

" import
if go#config#FoldEnable('import')
  syn region    goImport            start='import (' end=')' transparent fold contains=goImport,goString,goComment
else
  syn region    goImport            start='import (' end=')' transparent contains=goImport,goString,goComment
endif

" var, const
if go#config#FoldEnable('varconst')
  syn region    goVar               start='var ('   end='^\s*)$' transparent fold
                        \ contains=ALLBUT,goParen,goBlock,goFunction,goTypeName,goReceiverType,goReceiverVar,goArgumentName,goArgumentType,goSimpleArguments,goPointerOperator
  syn region    goConst             start='const (' end='^\s*)$' transparent fold
                        \ contains=ALLBUT,goParen,goBlock,goFunction,goTypeName,goReceiverType,goReceiverVar,goArgumentName,goArgumentType,goSimpleArguments,goPointerOperator
else
  syn region    goVar               start='var ('   end='^\s*)$' transparent
                        \ contains=ALLBUT,goParen,goBlock,goFunction,goTypeName,goReceiverType,goReceiverVar,goArgumentName,goArgumentType,goSimpleArguments,goPointerOperator
  syn region    goConst             start='const (' end='^\s*)$' transparent
                        \ contains=ALLBUT,goParen,goBlock,goFunction,goTypeName,goReceiverType,goReceiverVar,goArgumentName,goArgumentType,goSimpleArguments,goPointerOperator
endif

" Single-line var, const, and import.
syn match       goSingleDecl        /\(import\|var\|const\) [^(]\@=/ contains=goImport,goVar,goConst

" Integers
syn match       goDecimalInt        "\<-\=\d\+\%([Ee][-+]\=\d\+\)\=\>"
syn match       goHexadecimalInt    "\<-\=0[xX]\x\+\>"
syn match       goOctalInt          "\<-\=0\o\+\>"
syn match       goOctalError        "\<-\=0\o*[89]\d*\>"

hi def link     goDecimalInt        Integer
hi def link     goHexadecimalInt    Integer
hi def link     goOctalInt          Integer
hi def link     goOctalError        Error
hi def link     Integer             Number

" Floating point
syn match       goFloat             "\<-\=\d\+\.\d*\%([Ee][-+]\=\d\+\)\=\>"
syn match       goFloat             "\<-\=\.\d\+\%([Ee][-+]\=\d\+\)\=\>"

hi def link     goFloat             Float

" Imaginary literals
syn match       goImaginary         "\<-\=\d\+i\>"
syn match       goImaginary         "\<-\=\d\+[Ee][-+]\=\d\+i\>"
syn match       goImaginaryFloat    "\<-\=\d\+\.\d*\%([Ee][-+]\=\d\+\)\=i\>"
syn match       goImaginaryFloat    "\<-\=\.\d\+\%([Ee][-+]\=\d\+\)\=i\>"

hi def link     goImaginary         Number
hi def link     goImaginaryFloat    Float

" Spaces after "[]"
if go#config#HighlightArrayWhitespaceError()
  syn match goSpaceError display "\(\[\]\)\@<=\s\+"
endif

" Spacing errors around the 'chan' keyword
if go#config#HighlightChanWhitespaceError()
  " receive-only annotation on chan type
  "
  " \(\<chan\>\)\@<!<-  (only pick arrow when it doesn't come after a chan)
  " this prevents picking up 'chan<- chan<-' but not '<- chan'
  syn match goSpaceError display "\(\(\<chan\>\)\@<!<-\)\@<=\s\+\(\<chan\>\)\@="

  " send-only annotation on chan type
  "
  " \(<-\)\@<!\<chan\>  (only pick chan when it doesn't come after an arrow)
  " this prevents picking up '<-chan <-chan' but not 'chan <-'
  syn match goSpaceError display "\(\(<-\)\@<!\<chan\>\)\@<=\s\+\(<-\)\@="

  " value-ignoring receives in a few contexts
  syn match goSpaceError display "\(\(^\|[={(,;]\)\s*<-\)\@<=\s\+"
endif

" Extra types commonly seen
if go#config#HighlightExtraTypes()
  syn match goExtraType /\<bytes\.\(Buffer\)\>/
  syn match goExtraType /\<context\.\(Context\)\>/
  syn match goExtraType /\<io\.\(Reader\|ReadSeeker\|ReadWriter\|ReadCloser\|ReadWriteCloser\|Writer\|WriteCloser\|Seeker\)\>/
  syn match goExtraType /\<reflect\.\(Kind\|Type\|Value\)\>/
  syn match goExtraType /\<unsafe\.Pointer\>/
endif

" Space-tab error
if go#config#HighlightSpaceTabError()
  syn match goSpaceError display " \+\t"me=e-1
endif

" Trailing white space error
if go#config#HighlightTrailingWhitespaceError()
  syn match goSpaceError display excludenl "\s\+$"
endif

hi def link     goExtraType         Type
hi def link     goSpaceError        Error



" included from: https://github.com/athom/more-colorful.vim/blob/master/after/syntax/go.vim
"
" Comments; their contents
syn keyword     goTodo              contained NOTE
hi def link     goTodo              Todo

syn match goVarArgs /\.\.\./

" Operators;
if go#config#HighlightOperators()
  " match single-char operators:          - + % < > ! & | ^ * =
  " and corresponding two-char operators: -= += %= <= >= != &= |= ^= *= ==
  syn match goOperator /[-+%<>!&|^*=]=\?/
  " match / and /=
  syn match goOperator /\/\%(=\|\ze[^/*]\)/
  " match two-char operators:               << >> &^
  " and corresponding three-char operators: <<= >>= &^=
  syn match goOperator /\%(<<\|>>\|&^\)=\?/
  " match remaining two-char operators: := && || <- ++ --
  syn match goOperator /:=\|||\|<-\|++\|--/
  " match ...

  hi def link     goPointerOperator   goOperator
  hi def link     goVarArgs           goOperator
endif
hi def link     goOperator          Operator

" Functions;
if go#config#HighlightFunctions() || go#config#HighlightFunctionArguments()
  syn match goDeclaration       /\<func\>/ nextgroup=goReceiver,goFunction,goSimpleArguments skipwhite skipnl
  syn match goReceiverVar       /\w\+\ze\s\+\(\w\|\*\)/ nextgroup=goPointerOperator,goReceiverType skipwhite skipnl contained
  syn match goPointerOperator   /\*/ nextgroup=goReceiverType contained skipwhite skipnl
  syn match goFunction          /\w\+/ nextgroup=goSimpleArguments contained skipwhite skipnl
  syn match goReceiverType      /\w\+/ contained
if go#config#HighlightFunctionArguments()
  syn match goSimpleArguments   /(\(\w\|\_s\|[*\.\[\],\{\}<>-]\)*)/ contained contains=goArgumentName nextgroup=goSimpleArguments skipwhite skipnl
  syn match goArgumentName      /\w\+\(\s*,\s*\w\+\)*\ze\s\+\(\w\|\.\|\*\|\[\)/ contained nextgroup=goArgumentType skipwhite skipnl
  syn match goArgumentType      /\([^,)]\|\_s\)\+,\?/ contained nextgroup=goArgumentName skipwhite skipnl
                        \ contains=goVarArgs,goType,goSignedInts,goUnsignedInts,goFloats,goComplexes,goDeclType,goBlock
  hi def link   goReceiverVar       goArgumentName
  hi def link   goArgumentName      Identifier
endif
  syn match goReceiver          /(\s*\w\+\(\s\+\*\?\s*\w\+\)\?\s*)\ze\s*\w/ contained nextgroup=goFunction contains=goReceiverVar skipwhite skipnl
else
  syn keyword goDeclaration func
endif
hi def link     goFunction          Function

" Function calls;
if go#config#HighlightFunctionCalls()
  syn match goFunctionCall      /\w\+\ze(/ contains=goBuiltins,goDeclaration
endif
hi def link     goFunctionCall      Type

" Fields;
if go#config#HighlightFields()
  syn match goField                 /\.\w\+\([.\ \n\r\:\)\[,]\)\@=/hs=s+1
endif
hi def link    goField              Identifier

" Structs & Interfaces;
if go#config#HighlightTypes()
  syn match goTypeConstructor      /\<\w\+{\@=/
  syn match goTypeDecl             /\<type\>/ nextgroup=goTypeName skipwhite skipnl
  syn match goTypeName             /\w\+/ contained nextgroup=goDeclType skipwhite skipnl
  syn match goDeclType             /\<\(interface\|struct\)\>/ skipwhite skipnl
  hi def link     goReceiverType      Type
else
  syn keyword goDeclType           struct interface
  syn keyword goDeclaration        type
endif
hi def link     goTypeConstructor   Type
hi def link     goTypeName          Type
hi def link     goTypeDecl          Keyword
hi def link     goDeclType          Keyword

" Variable Assignments
if go#config#HighlightVariableAssignments()
  syn match goVarAssign /\v[_.[:alnum:]]+(,\s*[_.[:alnum:]]+)*\ze(\s*([-^+|^\/%&]|\*|\<\<|\>\>|\&\^)?\=[^=])/
  hi def link   goVarAssign         Special
endif

" Variable Declarations
if go#config#HighlightVariableDeclarations()
  syn match goVarDefs /\v\w+(,\s*\w+)*\ze(\s*:\=)/
  hi def link   goVarDefs           Special
endif

" Build Constraints
if go#config#HighlightBuildConstraints()
  syn match   goBuildKeyword      display contained "+build"
  " Highlight the known values of GOOS, GOARCH, and other +build options.
  syn keyword goBuildDirectives   contained
        \ android darwin dragonfly freebsd linux nacl netbsd openbsd plan9
        \ solaris windows 386 amd64 amd64p32 arm armbe arm64 arm64be ppc64
        \ ppc64le mips mipsle mips64 mips64le mips64p32 mips64p32le ppc
        \ s390 s390x sparc sparc64 cgo ignore race

  " Other words in the build directive are build tags not listed above, so
  " avoid highlighting them as comments by using a matchgroup just for the
  " start of the comment.
  " The rs=s+2 option lets the \s*+build portion be part of the inner region
  " instead of the matchgroup so it will be highlighted as a goBuildKeyword.
  syn region  goBuildComment      matchgroup=goBuildCommentStart
        \ start="//\s*+build\s"rs=s+2 end="$"
        \ contains=goBuildKeyword,goBuildDirectives
  hi def link goBuildCommentStart Comment
  hi def link goBuildDirectives   Type
  hi def link goBuildKeyword      PreProc
endif

if go#config#HighlightBuildConstraints() || go#config#FoldEnable('package_comment')
  " One or more line comments that are followed immediately by a "package"
  " declaration are treated like package documentation, so these must be
  " matched as comments to avoid looking like working build constraints.
  " The he, me, and re options let the "package" itself be highlighted by
  " the usual rules.
  exe 'syn region  goPackageComment    start=/\v(\/\/.*\n)+\s*package/'
        \ . ' end=/\v\n\s*package/he=e-7,me=e-7,re=e-7'
        \ . ' contains=@goCommentGroup,@Spell'
        \ . (go#config#FoldEnable('package_comment') ? ' fold' : '')
  exe 'syn region  goPackageComment    start=/\v^\s*\/\*.*\n(.*\n)*\s*\*\/\npackage/'
        \ . ' end=/\v\*\/\n\s*package/he=e-7,me=e-7,re=e-7'
        \ . ' contains=@goCommentGroup,@Spell'
        \ . (go#config#FoldEnable('package_comment') ? ' fold' : '')
  hi def link goPackageComment    Comment
endif

" :GoCoverage commands
hi def link goCoverageNormalText Comment

function! s:hi()
  hi def link goSameId Search

  " :GoCoverage commands
  hi def      goCoverageCovered    ctermfg=green guifg=#A6E22E
  hi def      goCoverageUncover    ctermfg=red guifg=#F92672

  " :GoDebug commands
  hi GoDebugBreakpoint term=standout ctermbg=117 ctermfg=0 guibg=#BAD4F5  guifg=Black
  hi GoDebugCurrent    term=reverse  ctermbg=12  ctermfg=7 guibg=DarkBlue guifg=White
endfunction

augroup vim-go-hi
  autocmd!
  autocmd ColorScheme * call s:hi()
augroup end
call s:hi()

" Search backwards for a global declaration to start processing the syntax.
"syn sync match goSync grouphere NONE /^\(const\|var\|type\|func\)\>/

" There's a bug in the implementation of grouphere. For now, use the
" following as a more expensive/less precise workaround.
syn sync minlines=500

let b:current_syntax = "go"

" vim: sw=2 ts=2 et
