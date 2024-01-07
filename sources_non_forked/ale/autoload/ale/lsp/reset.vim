" Author: w0rp <dev@w0rp.com>
" Description: Functions for resetting LSP servers.

function! s:Message(message) abort
    call ale#util#Execute('echom ' . string(a:message))
endfunction

" Stop all LSPs and remove all of the data for them.
function! ale#lsp#reset#StopAllLSPs() abort
    call ale#lsp#StopAll()

    if exists('*ale#definition#ClearLSPData')
        " Clear the go to definition mapping for everything.
        call ale#definition#ClearLSPData()
    endif

    if exists('*ale#lsp_linter#ClearLSPData')
        " Clear the mapping for connections, etc.
        call ale#lsp_linter#ClearLSPData()

        " Remove the problems for all of the LSP linters in every buffer.
        for l:buffer_string in keys(g:ale_buffer_info)
            let l:buffer = str2nr(l:buffer_string)

            " Non-ignored and disabled linters are included here so we can
            " clear results for them after we ignore or disable them.
            for l:linter in ale#linter#Get(getbufvar(l:buffer, '&filetype'))
                if !empty(l:linter.lsp)
                    call ale#engine#HandleLoclist(l:linter.name, l:buffer, [], 0)
                endif
            endfor
        endfor
    endif
endfunction

function! ale#lsp#reset#Complete(arg, line, pos) abort
    let l:linter_map = ale#lsp_linter#GetLSPLinterMap()
    let l:candidates = map(values(l:linter_map), {_, linter -> linter.name})
    call uniq(sort(l:candidates))
    call filter(l:candidates, {_, name -> name =~? a:arg})

    return l:candidates
endfunction

function! ale#lsp#reset#StopLSP(name, bang) abort
    let l:linter_map = ale#lsp_linter#GetLSPLinterMap()
    let l:matched = filter(
    \   items(l:linter_map),
    \   {_, item -> item[1].name is# a:name}
    \)

    if empty(l:matched)
        if a:bang isnot# '!'
            call s:Message('No running language server with name: ' . a:name)
        endif

        return
    endif

    " Stop LSP connections first.
    for [l:conn_id, l:linter] in l:matched
        call ale#lsp#Stop(l:conn_id)
    endfor

    if exists('*ale#definition#ClearLSPData')
        " Clear the go to definition mapping for everything.
        call ale#definition#ClearLSPData()
    endif

    " Remove connections from the lsp_linter map.
    for [l:conn_id, l:linter] in l:matched
        call remove(l:linter_map, l:conn_id)
    endfor

    " Remove the problems for the LSP linters in every buffer.
    for [l:buffer_string, l:info] in items(g:ale_buffer_info)
        let l:buffer = str2nr(l:buffer_string)
        let l:should_clear_buffer = 0

        for l:item in l:info.loclist
            if l:item.linter_name is# a:name
                let l:should_clear_buffer = 1

                break
            endif
        endfor

        if l:should_clear_buffer
            call ale#engine#HandleLoclist(a:name, l:buffer, [], 0)
        endif
    endfor
endfunction
