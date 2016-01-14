" Copyright 2009 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" go.vim: Vim syntax file for Go.
"
" Options:
"   There are some options for customizing the highlighting; the recommended
"   settings are the default values, but you can write:
"     let OPTION_NAME = 0
"   in your ~/.vimrc file to disable particular options. You can also write:
"     let OPTION_NAME = 1
"   to enable particular options.
"   At present, all options default to on, except highlight of:
"   functions, methods and structs.
"
"   - go_highlight_array_whitespace_error
"     Highlights white space after "[]".
"   - go_highlight_chan_whitespace_error
"     Highlights white space around the communications operator that don't follow
"     the standard style.
"   - go_highlight_extra_types
"     Highlights commonly used library types (io.Reader, etc.).
"   - go_highlight_space_tab_error
"     Highlights instances of tabs following spaces.
"   - go_highlight_trailing_whitespace_error
"     Highlights trailing white space.
"   - go_highlight_string_spellcheck
"     Specifies that strings should be spell checked

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

if !exists("g:go_highlight_array_whitespace_error")
  let g:go_highlight_array_whitespace_error = 1
endif

if !exists("g:go_highlight_chan_whitespace_error")
  let g:go_highlight_chan_whitespace_error = 1
endif

if !exists("g:go_highlight_extra_types")
  let g:go_highlight_extra_types = 1
endif

if !exists("g:go_highlight_space_tab_error")
  let g:go_highlight_space_tab_error = 1
endif

if !exists("g:go_highlight_trailing_whitespace_error")
  let g:go_highlight_trailing_whitespace_error = 1
endif

if !exists("g:go_highlight_operators")
  let g:go_highlight_operators = 0
endif

if !exists("g:go_highlight_functions")
  let g:go_highlight_functions = 0
endif

if !exists("g:go_highlight_methods")
  let g:go_highlight_methods = 0
endif

if !exists("g:go_highlight_structs")
  let g:go_highlight_structs = 0
endif

if !exists("g:go_highlight_build_constraints")
  let g:go_highlight_build_constraints = 0
endif

if !exists("g:go_highlight_string_spellcheck")
  let g:go_highlight_string_spellcheck = 1
endif

syn case match

syn keyword     goDirective         package import
syn keyword     goDeclaration       var const type
syn keyword     goDeclType          struct interface

hi def link     goDirective         Statement
hi def link     goDeclaration       Keyword
hi def link     goDeclType          Keyword

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

" Treat func specially: it's a declaration at the start of a line, but a type
" elsewhere. Order matters here.
syn match       goDeclaration       /\<func\>/


" Predefined functions and values
syn match       goBuiltins          /\<\v(append|cap|close|complex|copy|delete|imag|len)\ze\(/
syn match       goBuiltins          /\<\v(make|new|panic|print|println|real|recover)\ze\(/
syn keyword     goBoolean           iota true false nil

hi def link     goBuiltins          Keyword
hi def link     goBoolean           Boolean

" Comments; their contents
syn keyword     goTodo              contained TODO FIXME XXX BUG
syn cluster     goCommentGroup      contains=goTodo
syn region      goComment           start="/\*" end="\*/" contains=@goCommentGroup,@Spell
syn region      goComment           start="//" end="$" contains=@goCommentGroup,@Spell

hi def link     goComment           Comment
hi def link     goTodo              Todo

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
if g:go_highlight_string_spellcheck != 0
  syn region      goString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@goStringGroup,@Spell
  syn region      goRawString         start=+`+ end=+`+ contains=@Spell
else
  syn region      goString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@goStringGroup
  syn region      goRawString         start=+`+ end=+`+
endif
syn match       goFormatSpecifier   /%[-#0 +]*\%(\*\|\d\+\)\=\%(\.\%(\*\|\d\+\)\)*[vTtbcdoqxXUeEfgGsp]/ contained containedin=goString

hi def link     goString            String
hi def link     goRawString         String
hi def link     goFormatSpecifier   goSpecialString

" Characters; their contents
syn cluster     goCharacterGroup    contains=goEscapeOctal,goEscapeC,goEscapeX,goEscapeU,goEscapeBigU
syn region      goCharacter         start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=@goCharacterGroup

hi def link     goCharacter         Character

" Regions
syn region      goBlock             start="{" end="}" transparent fold
syn region      goParen             start='(' end=')' transparent

" Integers
syn match       goDecimalInt        "\<\d\+\([Ee]\d\+\)\?\>"
syn match       goHexadecimalInt    "\<0x\x\+\>"
syn match       goOctalInt          "\<0\o\+\>"
syn match       goOctalError        "\<0\o*[89]\d*\>"

hi def link     goDecimalInt        Integer
hi def link     goHexadecimalInt    Integer
hi def link     goOctalInt          Integer
hi def link     Integer             Number

" Floating point
syn match       goFloat             "\<\d\+\.\d*\([Ee][-+]\d\+\)\?\>"
syn match       goFloat             "\<\.\d\+\([Ee][-+]\d\+\)\?\>"
syn match       goFloat             "\<\d\+[Ee][-+]\d\+\>"

hi def link     goFloat             Float

" Imaginary literals
syn match       goImaginary         "\<\d\+i\>"
syn match       goImaginary         "\<\d\+\.\d*\([Ee][-+]\d\+\)\?i\>"
syn match       goImaginary         "\<\.\d\+\([Ee][-+]\d\+\)\?i\>"
syn match       goImaginary         "\<\d\+[Ee][-+]\d\+i\>"

hi def link     goImaginary         Number

" Spaces after "[]"
if g:go_highlight_array_whitespace_error != 0
  syn match goSpaceError display "\(\[\]\)\@<=\s\+"
endif

" Spacing errors around the 'chan' keyword
if g:go_highlight_chan_whitespace_error != 0
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
if g:go_highlight_extra_types != 0
  syn match goExtraType /\<bytes\.\(Buffer\)\>/
  syn match goExtraType /\<io\.\(Reader\|ReadSeeker\|ReadWriter\|ReadCloser\|ReadWriteCloser\|Writer\|WriteCloser\|Seeker\)\>/
  syn match goExtraType /\<reflect\.\(Kind\|Type\|Value\)\>/
  syn match goExtraType /\<unsafe\.Pointer\>/
endif

" Space-tab error
if g:go_highlight_space_tab_error != 0
  syn match goSpaceError display " \+\t"me=e-1
endif

" Trailing white space error
if g:go_highlight_trailing_whitespace_error != 0
  syn match goSpaceError display excludenl "\s\+$"
endif

hi def link     goExtraType         Type
hi def link     goSpaceError        Error



" included from: https://github.com/athom/more-colorful.vim/blob/master/after/syntax/go.vim
"
" Comments; their contents
syn keyword     goTodo              contained NOTE
hi def link     goTodo              Todo


" Operators;
if g:go_highlight_operators != 0
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
  syn match goOperator /\.\.\./
endif
hi def link     goOperator          Operator

" Functions;
if g:go_highlight_functions != 0
  syn match goFunction              /\(func\s\+\)\@<=\w\+\((\)\@=/
  syn match goFunction              /\()\s\+\)\@<=\w\+\((\)\@=/
endif
hi def link     goFunction          Function

" Methods;
if g:go_highlight_methods != 0
  syn match goMethod                /\(\.\)\@<=\w\+\((\)\@=/
endif
hi def link     goMethod            Type

" Structs;
if g:go_highlight_structs != 0
  syn match goStruct                /\(.\)\@<=\w\+\({\)\@=/
  syn match goStructDef             /\(type\s\+\)\@<=\w\+\(\s\+struct\s\+{\)\@=/
endif
hi def link     goStruct            Function
hi def link     goStructDef         Function

" Build Constraints
if g:go_highlight_build_constraints != 0
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

    " One or more line comments that are followed immediately by a "package"
    " declaration are treated like package documentation, so these must be
    " matched as comments to avoid looking like working build constraints.
    " The he, me, and re options let the "package" itself be highlighted by
    " the usual rules.
    syn region  goPackageComment    start=/\v(\/\/.*\n)+\s*package/
      \ end=/\v\n\s*package/he=e-7,me=e-7,re=e-7
      \ contains=@goCommentGroup,@Spell
    hi def link goPackageComment    Comment
endif


" Search backwards for a global declaration to start processing the syntax.
"syn sync match goSync grouphere NONE /^\(const\|var\|type\|func\)\>/

" There's a bug in the implementation of grouphere. For now, use the
" following as a more expensive/less precise workaround.
syn sync minlines=500

let b:current_syntax = "go"
