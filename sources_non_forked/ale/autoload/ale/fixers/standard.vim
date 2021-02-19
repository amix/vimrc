" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: Fixing files with Standard.

call ale#Set('javascript_standard_executable', 'standard')
call ale#Set('javascript_standard_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_standard_options', '')

function! ale#fixers#standard#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_standard', [
    \   'node_modules/standardx/bin/cmd.js',
    \   'node_modules/standard/bin/cmd.js',
    \   'node_modules/.bin/standard',
    \])
endfunction

function! ale#fixers#standard#Fix(buffer) abort
    let l:executable = ale#fixers#standard#GetExecutable(a:buffer)
    let l:filetype = getbufvar(a:buffer, '&filetype')
    let l:options_type = 'javascript_standard_options'

    if l:filetype =~# 'typescript'
        let l:options_type = 'typescript_standard_options'
    endif

    let l:options = ale#Var(a:buffer, l:options_type)

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --fix --stdin < %s > %t',
    \   'read_temporary_file': 1,
    \}
endfunction
