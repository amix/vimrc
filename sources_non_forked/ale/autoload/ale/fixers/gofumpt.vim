" Author: David Houston <houstdav000>
" Description: A stricter gofmt implementation.

call ale#Set('go_gofumpt_executable', 'gofumpt')
call ale#Set('go_gofumpt_options', '')

function! ale#fixers#gofumpt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'go_gofumpt_executable')
    let l:options = ale#Var(a:buffer, 'go_gofumpt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ale#Pad(l:options)
    \       . ' -w -- %t',
    \   'read_temporary_file': 1,
    \}
endfunction
