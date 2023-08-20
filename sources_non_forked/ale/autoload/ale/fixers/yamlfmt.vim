" Author: https://github.com/Spixmaster
" Description: Format YAML files with yamlfmt.

call ale#Set('yaml_yamlfmt_executable', 'yamlfmt')
call ale#Set('yaml_yamlfmt_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('yaml_yamlfmt_options', '')

function! ale#fixers#yamlfmt#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'yaml_yamlfmt',
    \   ['yamlfmt']
    \)

    let l:options = ale#Var(a:buffer, 'yaml_yamlfmt_options')

    return {
    \   'command': ale#Escape(l:executable) . ' ' . l:options . ' -in',
    \}
endfunction
