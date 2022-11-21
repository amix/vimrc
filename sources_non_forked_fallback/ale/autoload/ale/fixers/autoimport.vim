" Author: lyz-code
" Description: Fixing Python imports with autoimport.

call ale#Set('python_autoimport_executable', 'autoimport')
call ale#Set('python_autoimport_options', '')
call ale#Set('python_autoimport_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#autoimport#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'python_autoimport_options')

    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_autoimport',
    \   ['autoimport'],
    \)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'cwd': '%s:h',
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}
endfunction
