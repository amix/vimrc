" Author: Jeff Willette <jrwillette88@gmail.com>
" Description: Integration of goimports with ALE.

call ale#Set('go_goimports_executable', 'goimports')
call ale#Set('go_goimports_options', '')

function! ale#fixers#goimports#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'go_goimports_executable')
    let l:options = ale#Var(a:buffer, 'go_goimports_options')
    let l:env = ale#go#EnvString(a:buffer)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': l:env . ale#Escape(l:executable)
    \       . ' -l -w -srcdir %s'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
