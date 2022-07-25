if dracula#should_abort('perl')
    finish
endif

" Regex
hi! link perlMatchStartEnd       DraculaRed

" Builtin functions
hi! link perlOperator            DraculaCyan
hi! link perlStatementFiledesc   DraculaCyan
hi! link perlStatementFiles      DraculaCyan
hi! link perlStatementFlow       DraculaCyan
hi! link perlStatementHash       DraculaCyan
hi! link perlStatementIOfunc     DraculaCyan
hi! link perlStatementIPC        DraculaCyan
hi! link perlStatementList       DraculaCyan
hi! link perlStatementMisc       DraculaCyan
hi! link perlStatementNetwork    DraculaCyan
hi! link perlStatementNumeric    DraculaCyan
hi! link perlStatementProc       DraculaCyan
hi! link perlStatementPword      DraculaCyan
hi! link perlStatementRegexp     DraculaCyan
hi! link perlStatementScalar     DraculaCyan
hi! link perlStatementSocket     DraculaCyan
hi! link perlStatementTime       DraculaCyan
hi! link perlStatementVector     DraculaCyan

" Highlighting for quoting constructs, tied to existing option in vim-perl
if get(g:, 'perl_string_as_statement', 0)
  hi! link perlStringStartEnd DraculaRed
endif

" Signatures
hi! link perlSignature           DraculaOrangeItalic
hi! link perlSubPrototype        DraculaOrangeItalic

" Hash keys
hi! link perlVarSimpleMemberName DraculaPurple
