" gowork.vim: Vim syntax file for go.work file
"
" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax case match

" Reference documentation:
" https://go.dev/ref/mod#workspaces

" match keywords
syntax keyword goworkGo      go      contained
syntax keyword goworkUse use
syntax keyword goworkExclude exclude
syntax keyword goworkReplace replace
syntax keyword goworkRetract retract

" require, exclude, replace, and go can be also grouped into block
syntax region goworkUse start='require (' end=')' transparent contains=goworkUse,goworkPath
syntax region goworkReplace start='replace (' end=')' transparent contains=goworkReplace,goworkVersion
syntax match  goworkGo            '^go .*$'           transparent contains=goworkGo,goworkGoVersion

" set highlights
highlight default link goworkGo      Keyword
highlight default link goworkUse     Keyword
highlight default link goworkReplace Keyword

" comments are always in form of // ...
syntax region goworkComment  start="//" end="$" contains=@Spell
highlight default link goworkComment Comment

" make sure quoted import paths are higlighted
syntax region goworkString start=+"+ skip=+\\\\\|\\"+ end=+"+ 
highlight default link goworkString  String 

" replace operator is in the form of '=>'
syntax match goworkReplaceOperator "\v\=\>"
highlight default link goworkReplaceOperator Operator

" match go versions
syntax match goworkGoVersion "1\.\d\+" contained
highlight default link goworkGoVersion Identifier


" match paths in use directives
syntax match goworkPath "\f\+"

highlight default link goworkPath Identifier
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
syntax match goworkVersion "v\d\+\.\d\+\.\d\+\%(-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?"
"                          ^--- version ---^^------------ pre-release  ---------------------^^--------------- metadata ---------------------^
"   	                     ^--------------------------------------- semantic version -------------------------------------------------------^

" match pseudo versions
" without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef)
syntax match goworkVersion  "v\d\+\.0\.0-\d\{14\}-\x\+"
" when most recent version before target is a pre-release
syntax match goworkVersion  "v\d\+\.\d\+\.\d\+-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?\.0\.\d\{14}-\x\+"
"                          ^--- version ---^^--- ------ pre-release -----------------^^--------------- metadata ---------------------^
"                     	   ^------------------------------------- semantic version --------------------------------------------------^
" most recent version before the target is X.Y.Z
syntax match goworkVersion "v\d\+\.\d\+\.\d\+\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?-0\.\d\{14}-\x\+"
"                          ^--- version ---^^--------------- metadata ---------------------^

" match incompatible vX.Y.Z and their prereleases
syntax match goworkVersion "v[2-9]\{1}\d*\.\d\+\.\d\+\%(-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?+incompatible"
"                          ^------- version -------^^------------- pre-release ---------------------^^--------------- metadata ---------------------^
"               	         ^------------------------------------------- semantic version -----------------------------------------------------------^

" match incompatible pseudo versions
" incompatible without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef)
syntax match goworkVersion "v[2-9]\{1}\d*\.0\.0-\d\{14\}-\x\++incompatible"
" when most recent version before target is a pre-release
syntax match goworkVersion "v[2-9]\{1}\d*\.\d\+\.\d\+-\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\%(+\%([0-9A-Za-z-]\+\)\(\.[0-9A-Za-z-]\+\)*\)\?\.0\.\d\{14}-\x\++incompatible"
"                          ^------- version -------^^---------- pre-release -----------------^^--------------- metadata ---------------------^
"                     	   ^---------------------------------------- semantic version ------------------------------------------------------^
" most recent version before the target is X.Y.Z
syntax match goworkVersion "v[2-9]\{1}\d*\.\d\+\.\d\+\%(+\%([0-9A-Za-z-]\+\)\%(\.[0-9A-Za-z-]\+\)*\)\?-0\.\d\{14}-\x\++incompatible"
"                          ^------- version -------^^---------------- metadata ---------------------^
highlight default link goworkVersion Identifier

let b:current_syntax = "gowork"
