" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:fct = {
  \ 'Created': 1,
  \ 'Changed': 2,
  \ 'Deleted': 3,
\ }

function! go#lsp#filechangetype#FileChangeType(name)
	return s:fct[a:name]
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
