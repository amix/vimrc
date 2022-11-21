" Author: Yohei Yoshimuta <yoheimuta@gmail.com>
" Description: Integration of protolint with ALE.

call ale#Set('proto_protolint_executable', 'protolint')
call ale#Set('proto_protolint_config', '')

function! ale#fixers#protolint#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'proto_protolint_executable')

    return ale#Escape(l:executable)
endfunction

function! ale#fixers#protolint#Fix(buffer) abort
    let l:executable = ale#fixers#protolint#GetExecutable(a:buffer)
    let l:config = ale#Var(a:buffer, 'proto_protolint_config')

    return {
    \   'command': l:executable
    \       . (!empty(l:config) ? ' -config_path=' . ale#Escape(l:config) : '')
    \       . ' -fix'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction


