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


" highlight semver, note that this is very simple. But it works for now
syntax match gomodVersion "v\d\+\.\d\+\.\d\+"
syntax match gomodVersion "v\d\+\.\d\+\.\d\+-\S*"
syntax match gomodVersion "v\d\+\.\d\+\.\d\++incompatible"
highlight default link gomodVersion Identifier

let b:current_syntax = "gomod"
