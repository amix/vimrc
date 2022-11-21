" Author Pig Frown <pigfrown@protonmail.com>
" Description: Fix Go files long lines with golines"

call ale#Set('go_golines_executable', 'golines')

call ale#Set('go_golines_options', '')

function! ale#fixers#golines#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'go_golines_executable')
    let l:options = ale#Var(a:buffer, 'go_golines_options')
    let l:env = ale#go#EnvString(a:buffer)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': l:env . ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction
