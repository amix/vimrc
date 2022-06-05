" slimv-syntax-clojure.vim:
"               Clojure syntax plugin for Slimv
" Version:      0.9.11
" Last Change:  10 Jun 2013
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:current_syntax") || exists("g:slimv_disable_clojure")
  finish
endif

" Clojure keywords not defined by lisp.vim
syn keyword lispFunc def defmulti defn defn- defonce defprotocol doall dorun doseq dosync doto
syn keyword lispFunc filter fn for future in-ns letfn ns range str take try

" Try to load built-in or third party syntax files
" First clojure then lisp (if clojure not found) 
runtime syntax/**/clojure.vim
runtime syntax/**/lisp.vim

" Add [] and {} to the lisp_rainbow handling
syn match			 lispSymbol			  contained			   ![^()\[\]{}'`,"; \t]\+!
syn match			 lispBarSymbol			  contained			   !|..\{-}|!
syn match			 lispAtom			  "'[^ \t()\[\]{}]\+"		   contains=lispAtomMark
if exists("g:lisp_rainbow") && g:lisp_rainbow != 0
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

    silent! syn clear lispParen0
    silent! syn clear lispParen1
    silent! syn clear lispParen2
    silent! syn clear lispParen3
    silent! syn clear lispParen4
    silent! syn clear lispParen5
    silent! syn clear lispParen6
    silent! syn clear lispParen7
    silent! syn clear lispParen8
    silent! syn clear lispParen9

    syn region clojureSexp   matchgroup=hlLevel9 start="("  matchgroup=hlLevel9 end=")"  contains=TOP,@Spell
    syn region clojureParen0 matchgroup=hlLevel8 start="`\=(" end=")" contains=TOP,clojureParen0,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen1 matchgroup=hlLevel7 start="`\=(" end=")" contains=TOP,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen2 matchgroup=hlLevel6 start="`\=(" end=")" contains=TOP,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen3 matchgroup=hlLevel5 start="`\=(" end=")" contains=TOP,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen4 matchgroup=hlLevel4 start="`\=(" end=")" contains=TOP,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen5 matchgroup=hlLevel3 start="`\=(" end=")" contains=TOP,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen6 matchgroup=hlLevel2 start="`\=(" end=")" contains=TOP,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen7 matchgroup=hlLevel1 start="`\=(" end=")" contains=TOP,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen8 matchgroup=hlLevel0 start="`\=(" end=")" contains=TOP,clojureParen8,NoInParens

    syn region clojureVector matchgroup=hlLevel9 start="\[" matchgroup=hlLevel9 end="\]" contains=TOP,@Spell
    syn region clojureParen0 matchgroup=hlLevel8 start="`\=\[" end="\]" contains=TOP,clojureParen0,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen1 matchgroup=hlLevel7 start="`\=\[" end="\]" contains=TOP,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen2 matchgroup=hlLevel6 start="`\=\[" end="\]" contains=TOP,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen3 matchgroup=hlLevel5 start="`\=\[" end="\]" contains=TOP,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen4 matchgroup=hlLevel4 start="`\=\[" end="\]" contains=TOP,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen5 matchgroup=hlLevel3 start="`\=\[" end="\]" contains=TOP,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen6 matchgroup=hlLevel2 start="`\=\[" end="\]" contains=TOP,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen7 matchgroup=hlLevel1 start="`\=\[" end="\]" contains=TOP,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen8 matchgroup=hlLevel0 start="`\=\[" end="\]" contains=TOP,clojureParen8,NoInParens

    syn region clojureMap    matchgroup=hlLevel9 start="{"  matchgroup=hlLevel9 end="}"  contains=TOP,@Spell
    syn region clojureParen0 matchgroup=hlLevel8 start="`\={" end="}" contains=TOP,clojureParen0,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen1 matchgroup=hlLevel7 start="`\={" end="}" contains=TOP,clojureParen1,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen2 matchgroup=hlLevel6 start="`\={" end="}" contains=TOP,clojureParen2,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen3 matchgroup=hlLevel5 start="`\={" end="}" contains=TOP,clojureParen3,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen4 matchgroup=hlLevel4 start="`\={" end="}" contains=TOP,clojureParen4,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen5 matchgroup=hlLevel3 start="`\={" end="}" contains=TOP,clojureParen5,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen6 matchgroup=hlLevel2 start="`\={" end="}" contains=TOP,clojureParen6,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen7 matchgroup=hlLevel1 start="`\={" end="}" contains=TOP,clojureParen7,clojureParen8,NoInParens
    syn region clojureParen8 matchgroup=hlLevel0 start="`\={" end="}" contains=TOP,clojureParen8,NoInParens
endif

