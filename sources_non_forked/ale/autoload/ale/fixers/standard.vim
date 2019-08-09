" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: Fixing files with Standard.

call ale#Set('javascript_standard_executable', 'standard')
call ale#Set('javascript_standard_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_standard_options', '')

function! ale#fixers#standard#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_standard', [
    \   'node_modules/standard/bin/cmd.js',
    \   'node_modules/.bin/standard',
    \])
endfunction

function! ale#fixers#standard#Fix(buffer) abort
    let l:executable = ale#fixers#standard#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_standard_options')

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --fix %t',
    \   'read_temporary_file': 1,
    \}
endfunction
