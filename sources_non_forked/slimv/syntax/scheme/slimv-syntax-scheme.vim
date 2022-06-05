" slimv-syntax-scheme.vim:
"               Scheme syntax plugin for Slimv
" Version:      0.9.14
" Last Change:  27 Jun 2020
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:current_syntax") || exists("g:slimv_disable_scheme")
  finish
endif

runtime syntax/**/scheme.vim

" Add lisp_rainbow handling

syn keyword schemeExtSyntax     ->environment ->namestring
syn match   schemeExtSyntax     "#![-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
syn match   schemeAtomMark      "'"
syn match   schemeAtom          "'[^ \t()\[\]{}]\+" contains=schemeAtomMark

try
    " Check if using a more recent version of scheme.vim
    " that defines multi-line and datum comments
    silent syn list schemeDatumComment
    syn cluster schemeListCluster   contains=schemeSyntax,schemeFunc,schemeString,schemeCharacter,schemeNumber,schemeBoolean,schemeConstant,schemeComment,schemeMultilineComment,schemeDatumComment,schemeQuoted,schemeUnquote,schemeStrucRestricted,schemeOther,schemeError,schemeExtSyntax,schemeExtFunc,schemeAtom,schemeDelimiter
catch
    syn region  schemeMultilineComment  start=/#|/ end=/|#/ contains=schemeMultilineComment
    syn cluster schemeListCluster   contains=schemeSyntax,schemeFunc,schemeString,schemeCharacter,schemeNumber,schemeBoolean,schemeConstant,schemeComment,schemeMultilineComment,schemeQuoted,schemeUnquote,schemeStrucRestricted,schemeOther,schemeError,schemeExtSyntax,schemeExtFunc,schemeAtom,schemeDelimiter
endtry

hi def link schemeAtomMark      Delimiter
hi def link schemeAtom          Identifier


if exists("g:lisp_rainbow") && g:lisp_rainbow != 0
    syn region schemeParen0           matchgroup=hlLevel0 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen1
    syn region schemeParen1 contained matchgroup=hlLevel1 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen2
    syn region schemeParen2 contained matchgroup=hlLevel2 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen3
    syn region schemeParen3 contained matchgroup=hlLevel3 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen4
    syn region schemeParen4 contained matchgroup=hlLevel4 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen5
    syn region schemeParen5 contained matchgroup=hlLevel5 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen6
    syn region schemeParen6 contained matchgroup=hlLevel6 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen7
    syn region schemeParen7 contained matchgroup=hlLevel7 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen8
    syn region schemeParen8 contained matchgroup=hlLevel8 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen9
    syn region schemeParen9 contained matchgroup=hlLevel9 start="`\=(" end=")" skip="|.\{-}|" contains=@schemeListCluster,schemeParen0

    syn region schemeParen0           matchgroup=hlLevel0 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen1
    syn region schemeParen1 contained matchgroup=hlLevel1 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen2
    syn region schemeParen2 contained matchgroup=hlLevel2 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen3
    syn region schemeParen3 contained matchgroup=hlLevel3 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen4
    syn region schemeParen4 contained matchgroup=hlLevel4 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen5
    syn region schemeParen5 contained matchgroup=hlLevel5 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen6
    syn region schemeParen6 contained matchgroup=hlLevel6 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen7
    syn region schemeParen7 contained matchgroup=hlLevel7 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen8
    syn region schemeParen8 contained matchgroup=hlLevel8 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen9
    syn region schemeParen9 contained matchgroup=hlLevel9 start="`\=\[" end="\]" skip="|.\{-}|" contains=@schemeListCluster,schemeParen0

    syn region schemeParen0           matchgroup=hlLevel0 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen1
    syn region schemeParen1 contained matchgroup=hlLevel1 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen2
    syn region schemeParen2 contained matchgroup=hlLevel2 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen3
    syn region schemeParen3 contained matchgroup=hlLevel3 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen4
    syn region schemeParen4 contained matchgroup=hlLevel4 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen5
    syn region schemeParen5 contained matchgroup=hlLevel5 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen6
    syn region schemeParen6 contained matchgroup=hlLevel6 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen7
    syn region schemeParen7 contained matchgroup=hlLevel7 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen8
    syn region schemeParen8 contained matchgroup=hlLevel8 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen9
    syn region schemeParen9 contained matchgroup=hlLevel9 start="`\={" end="}" skip="|.\{-}|" contains=@schemeListCluster,schemeParen0

    if &bg == "dark"
        hi def hlLevel0 ctermfg=red         guifg=red1
        hi def hlLevel1 ctermfg=yellow      guifg=orange1
        hi def hlLevel2 ctermfg=green       guifg=yellow1
        hi def hlLevel3 ctermfg=cyan        guifg=greenyellow
        hi def hlLevel4 ctermfg=magenta     guifg=green1
        hi def hlLevel5 ctermfg=red         guifg=springgreen1
        hi def hlLevel6 ctermfg=yellow      guifg=cyan1
        hi def hlLevel7 ctermfg=green       guifg=slateblue1
        hi def hlLevel8 ctermfg=cyan        guifg=magenta1
        hi def hlLevel9 ctermfg=magenta     guifg=purple1
    else
        hi def hlLevel0 ctermfg=red         guifg=red3
        hi def hlLevel1 ctermfg=darkyellow  guifg=orangered3
        hi def hlLevel2 ctermfg=darkgreen   guifg=orange2
        hi def hlLevel3 ctermfg=blue        guifg=yellow3
        hi def hlLevel4 ctermfg=darkmagenta guifg=olivedrab4
        hi def hlLevel5 ctermfg=red         guifg=green4
        hi def hlLevel6 ctermfg=darkyellow  guifg=paleturquoise3
        hi def hlLevel7 ctermfg=darkgreen   guifg=deepskyblue4
        hi def hlLevel8 ctermfg=blue        guifg=darkslateblue
        hi def hlLevel9 ctermfg=darkmagenta guifg=darkviolet
    endif
endif

