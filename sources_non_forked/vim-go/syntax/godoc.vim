" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.

if exists("b:current_syntax")
  finish
endif

syn case match

syn match   godocTitle        "^\([A-Z][A-Z ]*\)$"
hi def link godocTitle        Title

" Single Line Definitions
syn match   godocMethodRec    /\i\+\ze)/ contained
syn match   godocMethodName   /) \zs\i\+\ze(/ contained
syn match   godocMethod       /^func \((\i\+ [^)]*)\) \i\+(/ contains=godocMethodRec,godocMethodName
syn match   godocFunction     /^func \zs\i\+\ze(/

syn match   godocType         /^type \zs\i\+\ze.*/
syn match   godocVar          /^var \zs\i\+\ze.*/
syn match   godocConst        /^const \zs\i\+\ze.*/

hi def link godocMethodRec    Type
hi def link godocType         Type
hi def link godocMethodName   Function
hi def link godocFunction     Function
hi def link godocVar          Identifier
hi def link godocConst        Identifier

" Definition Blocks
syn region  godocComment      start="/\*" end="\*/" contained
syn region  godocComment      start="//" end="$" contained
syn match   godocDefinition   /^\s\+\i\+/ contained

syn region  godocVarBlock     start=/^var (/ end=/^)/ contains=godocComment,godocDefinition
syn region  godocConstBlock   start=/^const (/ end=/^)/ contains=godocComment,godocDefinition
syn region  godocTypeBlock    start=/^type \i\+ \(interface\|struct\) {/ end=/^}/ matchgroup=godocType contains=godocComment,godocType

hi def link godocComment      Comment
hi def link godocDefinition   Identifier

syn sync minlines=500

let b:current_syntax = "godoc"

" vim:ts=4 sts=2 sw=2:
