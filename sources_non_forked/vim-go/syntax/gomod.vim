" gomod.vim: Vim syntax file for go.mod file
"
" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax case match

" Reference documentation:
" https://golang.org/ref/mod#go-mod-file-grammar

" match keywords
syntax keyword gomodModule  module
syntax keyword gomodGo      go      contained
syntax keyword gomodRequire require
syntax keyword gomodExclude exclude
syntax keyword gomodReplace replace
syntax keyword gomodRetract retract

" require, exclude, replace, and go can be also grouped into block
syntax region gomodRequire start='require (' end=')' transparent contains=gomodRequire,gomodVersion
syntax region gomodExclude start='exclude (' end=')' transparent contains=gomodExclude,gomodVersion
syntax region gomodReplace start='replace (' end=')' transparent contains=gomodReplace,gomodVersion
syntax region gomodRetract start='retract (' end=')' transparent contains=gomodVersionRange,gomodVersion
syntax match  gomodGo            '^go .*$'           transparent contains=gomodGo,gomodGoVersion

" set highlights
highlight default link gomodModule  Keyword
highlight default link gomodGo      Keyword
highlight default link gomodRequire Keyword
highlight default link gomodExclude Keyword
highlight default link gomodReplace Keyword
highlight default link gomodRetract Keyword

" comments are always in form of // ...
syntax region gomodComment  start="//" end="$" contains=@Spell
highlight default link gomodComment Comment

" make sure quoted import paths are higlighted
syntax region gomodString start=+"+ skip=+\\\\\|\\"+ end=+"+ 
highlight default link gomodString  String 

" replace operator is in the form of '=>'
syntax match gomodReplaceOperator "\v\=\>"
highlight default link gomodReplaceOperator Operator

" match go versions
syntax match gomodGoVersion "1\.\d\+" contained
highlight default link gomodGoVersion Identifier

" highlight versions:
"  * vX.Y.Z-pre
"  * vX.Y.Z
"  * vX.0.0-yyyyymmddhhmmss-abcdefabcdef
"  * vX.Y.Z-pre.0.yyyymmddhhmmss-abcdefabcdef
"  * vX.Y.(Z+1)-0.yyyymmddhhss-abcdefabcdef
"  see https://godoc.org/golang.org/x/tools/internal/semver for more
"  information about how semantic versions are parsed and
"  https://golang.org/cmd/go/ for how pseudo-versions and +incompatible
"  are applied.


" match vX.Y.Z and their prereleases
syntax match gomodVersion "v\d\+\.\d\+\.\d\+\%(-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?"
"                          ^--- version ---^^------------ pre-release  ---------------------^^--------------- metadata ---------------------^
"   	                     ^--------------------------------------- semantic version -------------------------------------------------------^

" match pseudo versions
" without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef)
syntax match gomodVersion  "v\d\+\.0\.0-\d\{14\}-\x\+"
" when most recent version before target is a pre-release
syntax match gomodVersion  "v\d\+\.\d\+\.\d\+-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?\.0\.\d\{14}-\x\+"
"                          ^--- version ---^^--- ------ pre-release -----------------^^--------------- metadata ---------------------^
"                     	   ^------------------------------------- semantic version --------------------------------------------------^
" most recent version before the target is X.Y.Z
syntax match gomodVersion "v\d\+\.\d\+\.\d\+\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?-0\.\d\{14}-\x\+"
"                          ^--- version ---^^--------------- metadata ---------------------^

" match incompatible vX.Y.Z and their prereleases
syntax match gomodVersion "v[2-9]\{1}\d*\.\d\+\.\d\+\%(-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?+incompatible"
"                          ^------- version -------^^------------- pre-release ---------------------^^--------------- metadata ---------------------^
"               	         ^------------------------------------------- semantic version -----------------------------------------------------------^

" match incompatible pseudo versions
" incompatible without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef)
syntax match gomodVersion "v[2-9]\{1}\d*\.0\.0-\d\{14\}-\x\++incompatible"
" when most recent version before target is a pre-release
syntax match gomodVersion "v[2-9]\{1}\d*\.\d\+\.\d\+-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?\.0\.\d\{14}-\x\++incompatible"
"                          ^------- version -------^^---------- pre-release -----------------^^--------------- metadata ---------------------^
"                     	   ^---------------------------------------- semantic version ------------------------------------------------------^
" most recent version before the target is X.Y.Z
syntax match gomodVersion "v[2-9]\{1}\d*\.\d\+\.\d\+\%(+\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?-0\.\d\{14}-\x\++incompatible"
"                          ^------- version -------^^---------------- metadata ---------------------^
highlight default link gomodVersion Identifier

" match go version ranges in retract directive
" https://golang.org/ref/mod#go-mod-file-retract
syntax region gomodVersionRange start="\[" end="\]" transparent matchgroup=gomodVersionRangeBracket contains=gomodVersion,gomodVersionRangeSeparator
highlight default link gomodVersionRange Operator
syntax match gomodVersionRangeBracket "\[" contained
syntax match gomodVersionRangeBracket "\]" contained
highlight default link gomodVersionRangeBracket Operator
syntax match gomodVersionRangeSeparator "," contained
highlight default link gomodVersionRangeSeparator Operator


let b:current_syntax = "gomod"
