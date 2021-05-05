" Author: lyz-code
" Description: Fixing yaml files with yamlfix.

call ale#Set('yaml_yamlfix_executable', 'yamlfix')
call ale#Set('yaml_yamlfix_options', '')
call ale#Set('yaml_yamlfix_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#yamlfix#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'yaml_yamlfix_options')
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'yaml_yamlfix',
    \   ['yamlfix'],
    \)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'cwd': '%s:h',
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
