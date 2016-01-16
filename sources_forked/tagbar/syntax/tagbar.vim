" File:        tagbar.vim
" Description: Tagbar syntax settings
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.6.1

scriptencoding utf-8

if exists("b:current_syntax")
    finish
endif

let s:ics = escape(join(g:tagbar_iconchars, ''), ']^\-')
let s:pattern = '\(^[' . s:ics . '] \?\)\@<=[^-+: ]\+[^:]\+$'
execute "syntax match TagbarKind '" . s:pattern . "'"

let s:pattern = '\(\S\@<![' . s:ics . '][-+# ]\?\)\@<=[^*(]\+\(\*\?\(([^)]\+)\)\? :\)\@='
execute "syntax match TagbarScope '" . s:pattern . "'"

let s:pattern = '\S\@<![' . s:ics . ']\([-+# ]\?\)\@='
execute "syntax match TagbarFoldIcon '" . s:pattern . "'"

let s:pattern = '\(\S\@<![' . s:ics . ' ]\)\@<=+\([^-+# ]\)\@='
execute "syntax match TagbarVisibilityPublic '" . s:pattern . "'"
let s:pattern = '\(\S\@<![' . s:ics . ' ]\)\@<=#\([^-+# ]\)\@='
execute "syntax match TagbarVisibilityProtected '" . s:pattern . "'"
let s:pattern = '\(\S\@<![' . s:ics . ' ]\)\@<=-\([^-+# ]\)\@='
execute "syntax match TagbarVisibilityPrivate '" . s:pattern . "'"

unlet s:pattern

syntax match TagbarHelp      '^".*' contains=TagbarHelpKey,TagbarHelpTitle
syntax match TagbarHelpKey   '" \zs.*\ze:' contained
syntax match TagbarHelpTitle '" \zs-\+ \w\+ -\+' contained

syntax match TagbarNestedKind '^\s\+\[[^]]\+\]$'
syntax match TagbarType       ' : \zs.*'
syntax match TagbarSignature  '(.*)'
syntax match TagbarPseudoID   '\*\ze :'

highlight default link TagbarHelp       Comment
highlight default link TagbarHelpKey    Identifier
highlight default link TagbarHelpTitle  PreProc
highlight default link TagbarKind       Identifier
highlight default link TagbarNestedKind TagbarKind
highlight default link TagbarScope      Title
highlight default link TagbarType       Type
highlight default link TagbarSignature  SpecialKey
highlight default link TagbarPseudoID   NonText
highlight default link TagbarFoldIcon   Statement
highlight default link TagbarHighlight  Search

highlight default TagbarAccessPublic    guifg=Green ctermfg=Green
highlight default TagbarAccessProtected guifg=Blue  ctermfg=Blue
highlight default TagbarAccessPrivate   guifg=Red   ctermfg=Red
highlight default link TagbarVisibilityPublic    TagbarAccessPublic
highlight default link TagbarVisibilityProtected TagbarAccessProtected
highlight default link TagbarVisibilityPrivate   TagbarAccessPrivate

let b:current_syntax = "tagbar"

" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
