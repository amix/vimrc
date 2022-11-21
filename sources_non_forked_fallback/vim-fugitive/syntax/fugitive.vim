if exists("b:current_syntax")
  finish
endif

syn sync fromstart
syn spell notoplevel

syn include @fugitiveDiff syntax/diff.vim

syn match fugitiveHeader /^[A-Z][a-z][^:]*:/
syn match fugitiveHeader /^Head:/ nextgroup=fugitiveHash,fugitiveSymbolicRef skipwhite
syn match fugitiveHeader /^Pull:\|^Rebase:\|^Merge:\|^Push:/ nextgroup=fugitiveSymbolicRef skipwhite
syn match fugitiveHelpHeader /^Help:/ nextgroup=fugitiveHelpTag skipwhite
syn match fugitiveHelpTag    /\S\+/ contained

syn region fugitiveSection start=/^\%(.*(\d\++\=)$\)\@=/ contains=fugitiveHeading end=/^$/
syn cluster fugitiveSection contains=fugitiveSection
syn match fugitiveHeading /^[A-Z][a-z][^:]*\ze (\d\++\=)$/ contains=fugitivePreposition contained nextgroup=fugitiveCount skipwhite
syn match fugitiveCount /(\d\++\=)/hs=s+1,he=e-1 contained
syn match fugitivePreposition /\<\%([io]nto\|from\|to\|Rebasing\%( detached\)\=\)\>/ transparent contained nextgroup=fugitiveHash,fugitiveSymbolicRef skipwhite

syn match fugitiveInstruction /^\l\l\+\>/ contained containedin=@fugitiveSection nextgroup=fugitiveHash skipwhite
syn match fugitiveDone /^done\>/ contained containedin=@fugitiveSection nextgroup=fugitiveHash skipwhite
syn match fugitiveStop /^stop\>/ contained containedin=@fugitiveSection nextgroup=fugitiveHash skipwhite
syn match fugitiveModifier /^[MADRCU?]\{1,2} / contained containedin=@fugitiveSection
syn match fugitiveSymbolicRef /\.\@!\%(\.\.\@!\|[^[:space:][:cntrl:]\:.]\)\+\.\@<!/ contained
syn match fugitiveHash /^\x\{4,\}\S\@!/ contained containedin=@fugitiveSection
syn match fugitiveHash /\S\@<!\x\{4,\}\S\@!/ contained

syn region fugitiveHunk start=/^\%(@@\+ -\)\@=/ end=/^\%([A-Za-z?@]\|$\)\@=/ contains=diffLine,diffRemoved,diffAdded,diffNoEOL containedin=@fugitiveSection fold

for s:section in ['Untracked', 'Unstaged', 'Staged']
  exe 'syn region fugitive' . s:section . 'Section start=/^\%(' . s:section . ' .*(\d\++\=)$\)\@=/ contains=fugitive' . s:section . 'Heading end=/^$/'
  exe 'syn match fugitive' . s:section . 'Modifier /^[MADRCU?] / contained containedin=fugitive' . s:section . 'Section'
  exe 'syn cluster fugitiveSection add=fugitive' . s:section . 'Section'
  exe 'syn match fugitive' . s:section . 'Heading /^[A-Z][a-z][^:]*\ze (\d\++\=)$/ contains=fugitivePreposition contained nextgroup=fugitiveCount skipwhite'
endfor
unlet s:section

hi def link fugitiveHelpHeader fugitiveHeader
hi def link fugitiveHeader Label
hi def link fugitiveHelpTag Tag
hi def link fugitiveHeading PreProc
hi def link fugitiveUntrackedHeading PreCondit
hi def link fugitiveUnstagedHeading Macro
hi def link fugitiveStagedHeading Include
hi def link fugitiveModifier Type
hi def link fugitiveUntrackedModifier StorageClass
hi def link fugitiveUnstagedModifier Structure
hi def link fugitiveStagedModifier Typedef
hi def link fugitiveInstruction Type
hi def link fugitiveStop Function
hi def link fugitiveHash Identifier
hi def link fugitiveSymbolicRef Function
hi def link fugitiveCount Number

let b:current_syntax = "fugitive"
