" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#calls#Callers() abort
  if !go#config#GoplsEnabled()
    call go#util#EchoError("gopls is disabled")
    return
  endif
  let [l:line, l:col] = getpos('.')[1:2]
  let [l:line, l:col] = go#lsp#lsp#Position(l:line, l:col)
  let l:fname = expand('%:p')
  call go#lsp#Callers(l:fname, l:line, l:col, funcref('s:parse_output', ['callers']))
  return
endfunction

" This uses Vim's errorformat to parse the output and put it into a quickfix
" or locationlist.
function! s:parse_output(mode, output) abort
  let errformat = ",%f:%l:%c:\ %m"
  let l:listtype = go#list#Type("GoCallers")
  call go#list#ParseFormat(l:listtype, errformat, a:output, a:mode, 0)

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
