" Tell ALE that another source has started checking a buffer.
function! ale#other_source#StartChecking(buffer, linter_name) abort
    call ale#engine#InitBufferInfo(a:buffer)
    let l:list = g:ale_buffer_info[a:buffer].active_other_sources_list

    call add(l:list, a:linter_name)
    call uniq(sort(l:list))
endfunction

" Show some results, and stop checking a buffer.
" To clear results or cancel checking a buffer, an empty List can be given.
function! ale#other_source#ShowResults(buffer, linter_name, loclist) abort
    call ale#engine#InitBufferInfo(a:buffer)
    let l:info = g:ale_buffer_info[a:buffer]

    " Remove this linter name from the active list.
    let l:list = l:info.active_other_sources_list
    call filter(l:list, 'v:val isnot# a:linter_name')

    call ale#engine#HandleLoclist(a:linter_name, a:buffer, a:loclist, 1)
endfunction
