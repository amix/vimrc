" Description: Fixer for rego files

call ale#Set('opa_fmt_executable', 'opa')
call ale#Set('opa_fmt_options', '')

function! ale#fixers#opafmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'opa_fmt_executable')
    let l:options = ale#Var(a:buffer, 'opa_fmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' fmt'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction
