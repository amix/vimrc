" gomod.vim: Vim syntax file for go.mod file
"
" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax case match

" match keywords
syntax keyword gomodModule  module
syntax keyword gomodRequire require
syntax keyword gomodExclude exclude
syntax keyword gomodReplace replace

" require, exclude and replace can be also grouped into block
syntax region gomodRequire start='require (' end=')' transparent contains=gomodRequire,gomodVersion
syntax region gomodExclude start='exclude (' end=')' transparent contains=gomodExclude,gomodVersion
syntax region gomodReplace start='replace (' end=')' transparent contains=gomodReplace,gomodVersion

" set highlights
highlight default link gomodModule  Keyword
highlight default link gomodRequire Keyword
highlight default link gomodExclude Keyword
highlight default link gomodReplace Keyword

" comments are always in form of // ...
syntax region gomodComment  start="//" end="$" contains=@Spell
highlight default link gomodComment Comment

" make sure quoted import paths are higlighted
syntax region gomodString start=+"+ skip=+\\\\\|\\"+ end=+"+ 
highlight default link gomodString  String 

" replace operator is in the form of '=>'
syntax match gomodReplaceOperator "\v\=\>"
highlight default link gomodReplaceOperator Operator


" highlight versions:
"  * vX.Y.Z
"  * vX.0.0-yyyyymmddhhmmss-abcdefabcdef
"  * vX.Y.Z-pre.0.yyyymmddhhmmss-abcdefabcdef
"  * vX.Y.(Z+1)-0.yyyymmddhhss-abcdefabcdef
"  * +incompatible suffix when X > 1
" match vX.Y.Z and their prereleases
syntax match gomodVersion "v\d\+\.\d\+\.\d\+\%(-\%(\w\+\.\)\+0\.\d\{14}-\x\+\)\?"
" match target when most recent version before the target is X.Y.Z
syntax match gomodVersion "v\d\+\.\d\+\.[1-9]\{1}\d*\%(-0\.\%(\d\{14}-\x\+\)\)\?"
" match target without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef)
syntax match gomodVersion "v\d\+\.0\.0-\d\{14\}-\x\+"

" match vX.Y.Z and their prereleases for X>1
syntax match gomodVersion "v[2-9]\{1}\d\?\.\d\+\.\d\+\%(-\%(\w\+\.\)\+0\.\d\{14\}-\x\+\)\?\%(+incompatible\>\)\?"
" match target when most recent version before the target is X.Y.Z for X>1
syntax match gomodVersion "v[2-9]\{1}\d\?\.\d\+\.[1-9]\{1}\d*\%(-0\.\%(\d\{14\}-\x\+\)\)\?\%(+incompatible\>\)\?"
" match target without a major version before the commit (e.g.  vX.0.0-yyyymmddhhmmss-abcdefabcdef) for X>1
syntax match gomodVersion "v[2-9]\{1}\d\?\.0\.0-\d\{14\}-\x\+\%(+incompatible\>\)\?"

highlight default link gomodVersion Identifier

let b:current_syntax = "gomod"
