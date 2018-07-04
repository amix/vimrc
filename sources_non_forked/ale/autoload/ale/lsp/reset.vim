" Stop all LSPs and remove all of the data for them.
function! ale#lsp#reset#StopAllLSPs() abort
    call ale#lsp#StopAll()

    if exists('*ale#definition#ClearLSPData')
        " Clear the mapping for connections, etc.
        call ale#definition#ClearLSPData()
    endif

    if exists('*ale#lsp_linter#ClearLSPData')
        " Clear the mapping for connections, etc.
        call ale#lsp_linter#ClearLSPData()

        " Remove the problems for all of the LSP linters in every buffer.
        for l:buffer_string in keys(g:ale_buffer_info)
            let l:buffer = str2nr(l:buffer_string)

            for l:linter in ale#linter#Get(getbufvar(l:buffer, '&filetype'))
                if !empty(l:linter.lsp)
                    call ale#engine#HandleLoclist(l:linter.name, l:buffer, [])
                endif
            endfor
        endfor
    endif
endfunction
