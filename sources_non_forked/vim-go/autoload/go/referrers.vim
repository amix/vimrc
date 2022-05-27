" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#referrers#Referrers(selected) abort
  let l:mode = go#config#ReferrersMode()
  if l:mode == 'guru'
		call go#guru#Referrers(a:selected)
    return
  elseif l:mode == 'gopls'
    if !go#config#GoplsEnabled()
      call go#util#EchoError("go_referrers_mode is 'gopls', but gopls is disabled")
      return
    endif
    let [l:line, l:col] = getpos('.')[1:2]
    let [l:line, l:col] = go#lsp#lsp#Position(l:line, l:col)
    let l:fname = expand('%:p')
    call go#lsp#Referrers(l:fname, l:line, l:col, funcref('s:parse_output'))
    return
  else
    call go#util#EchoWarning('unknown value for g:go_referrers_mode')
  endif
endfunction

" This uses Vim's errorformat to parse the output and put it into a quickfix
" or locationlist.
function! s:parse_output(exit_val, output, title) abort
  if a:exit_val
    call go#util#EchoError(a:output)
    return
  endif

  let errformat = ",%f:%l:%c:\ %m"
  let l:listtype = go#list#Type("GoReferrers")
  call go#list#ParseFormat(l:listtype, errformat, a:output, a:title, 0)

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
endfunction
" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
