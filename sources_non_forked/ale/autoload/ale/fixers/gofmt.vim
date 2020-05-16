" Author: aliou <code@aliou.me>
" Description: Integration of gofmt with ALE.

call ale#Set('go_gofmt_executable', 'gofmt')
call ale#Set('go_gofmt_options', '')

function! ale#fixers#gofmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'go_gofmt_executable')
    let l:options = ale#Var(a:buffer, 'go_gofmt_options')
    let l:env = ale#go#EnvString(a:buffer)

    return {
    \   'command': l:env . ale#Escape(l:executable)
    \       . ' -l -w'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
