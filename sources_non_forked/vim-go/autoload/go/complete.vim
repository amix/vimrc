" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

" go#complete#GoInfo returns the description of the identifier under the
" cursor.
function! go#complete#GetInfo() abort
  return go#lsp#GetInfo()
endfunction

function! go#complete#Complete(findstart, base) abort
  if !go#config#GoplsEnabled()
    return -3
  endif

  let l:state = {'done': 0, 'matches': [], 'start': -1}

  function! s:handler(state, start, matches) abort dict
    let a:state.start = a:start
    let a:state.matches = a:matches
    let a:state.done = 1
  endfunction

  "findstart = 1 when we need to get the start of the match
  if a:findstart == 1
    let [l:line, l:col] = getpos('.')[1:2]
    let [l:line, l:col] = go#lsp#lsp#Position(l:line, l:col)
    let l:completion = go#lsp#Completion(expand('%:p'), l:line, l:col, funcref('s:handler', [l:state]))
    if l:completion
      return -3
    endif

    while !l:state.done
      sleep 10m
    endwhile

    if len(l:state.matches) == 0
      " no matches. cancel and leave completion mode.
      call go#util#EchoInfo("no matches")
      return -3
    endif

    let s:completions = l:state.matches

    return go#lsp#lsp#PositionOf(getline(l:line+1), l:state.start-1)

  else "findstart = 0 when we need to return the list of completions
    return s:completions
  endif
endfunction

function! go#complete#ToggleAutoTypeInfo() abort
  if go#config#AutoTypeInfo()
    call go#config#SetAutoTypeInfo(0)
    call go#util#EchoProgress("auto type info disabled")
  else
    call go#config#SetAutoTypeInfo(1)
    call go#util#EchoProgress("auto type info enabled")
  endif
  call go#auto#update_autocmd()
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
