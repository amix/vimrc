" Author: Albert Marquez - https://github.com/a-marquez
" Description: Fixing files with XO.

call ale#Set('javascript_xo_executable', 'xo')
call ale#Set('javascript_xo_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_xo_options', '')

function! ale#fixers#xo#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_xo', [
    \   'node_modules/xo/cli.js',
    \   'node_modules/.bin/xo',
    \])
endfunction

function! ale#fixers#xo#Fix(buffer) abort
    let l:executable = ale#fixers#xo#GetExecutable(a:buffer)

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \       . ' --fix %t',
    \   'read_temporary_file': 1,
    \}
endfunction
