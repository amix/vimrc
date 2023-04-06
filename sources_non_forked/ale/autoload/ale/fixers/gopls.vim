" Author: Sean Enck <enckse@voidedtech.com>
" Description: Integration of gopls format with ALE.

call ale#Set('go_gopls_fix_executable', 'gopls')
call ale#Set('go_gopls_fix_options', '')

function! ale#fixers#gopls#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'go_gopls_fix_executable')
    let l:options = ale#Var(a:buffer, 'go_gopls_fix_options')
    let l:env = ale#go#EnvString(a:buffer)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': l:env . ale#Escape(l:executable)
    \       . ' format'
    \       . ale#Pad(l:options)
    \       . ' -l -w %t',
    \   'read_temporary_file': 1,
    \}
endfunction
