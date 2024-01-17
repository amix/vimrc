scriptencoding utf-8

if exists("b:current_syntax")
  finish
endif

let s:subtype = matchstr(&l:filetype, '\<copilot\.\zs[[:alnum:]_-]\+')
if !empty(s:subtype) && s:subtype !=# 'copilot'
  exe 'syn include @copilotLanguageTop syntax/' . s:subtype . '.vim'
  unlet! b:current_syntax
endif

syn region copilotHeader start="\%^" end="^─\@="
syn region copilotSolution matchgroup=copilotSeparator start="^─\{9,}$" end="\%(^─\{9,\}$\)\@=\|\%$" keepend contains=@copilotLanguageTop

hi def link copilotHeader PreProc
hi def link copilotSeparator Comment

let b:current_syntax = "copilot"
