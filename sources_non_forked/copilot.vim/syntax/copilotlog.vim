scriptencoding utf-8

if exists("b:current_syntax")
  finish
endif

let s:subtype = matchstr(&l:filetype, '\<copilot\.\zs[[:alnum:]_-]\+')
if !empty(s:subtype) && s:subtype !=# 'copilot'
  exe 'syn include @copilotLanguageTop syntax/' . s:subtype . '.vim'
  unlet! b:current_syntax
endif

syn match copilotlogError '\[ERROR\]'
syn match copilotlogWarn '\[WARN\]'
syn match copilotlogInfo '\[INFO\]'
syn match copilotlogDebug '\[DEBUG\]'
syn match copilotlogTime '^\[\d\d\d\d-\d\d-\d\d.\d\d:\d\d:\d\d\]' nextgroup=copilotlogError,copilotlogWarn,copilotLogInfo,copilotLogDebug skipwhite

hi def link copilotlogTime NonText
hi def link copilotlogError ErrorMsg
hi def link copilotlogWarn WarningMsg
hi def link copilotlogInfo MoreMsg
hi def link copilotlogDebug ModeMsg

let b:current_syntax = "copilotlog"
