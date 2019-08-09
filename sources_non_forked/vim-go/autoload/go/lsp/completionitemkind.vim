" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:Text = 1
let s:Method = 2
let s:Function = 3
let s:Constructor = 4
let s:Field = 5
let s:Variable = 6
let s:Class = 7
let s:Interface = 8
let s:Module = 9
let s:Property = 10
let s:Unit = 11
let s:Value = 12
let s:Enum = 13
let s:Keyword = 14
let s:Snippet = 15
let s:Color = 16
let s:File = 17
let s:Reference = 18
let s:Folder = 19
let s:EnumMember = 20
let s:Constant = 21
let s:Struct = 22
let s:Event = 23
let s:Operator = 24
let s:TypeParameter = 25

function! go#lsp#completionitemkind#Vim(kind)
  if a:kind == s:Method || a:kind == s:Function || a:kind == s:Constructor
    return 'f'
  elseif a:kind == s:Variable || a:kind == s:Constant
    return 'v'
  elseif a:kind == s:Field || a:kind == s:Property
    return 'm'
  elseif a:kind == s:Class || a:kind == s:Interface || a:kind == s:Struct
    return 't'
  endif
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
