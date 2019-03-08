" Author: Ahmed El Gabri <@ahmedelgabri>
" Description: standardjs for JavaScript files

call ale#Set('javascript_standard_executable', 'standard')
call ale#Set('javascript_standard_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('javascript_standard_options', '')

function! ale_linters#javascript#standard#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_standard', [
    \   'node_modules/standard/bin/cmd.js',
    \   'node_modules/.bin/standard',
    \])
endfunction

function! ale_linters#javascript#standard#GetCommand(buffer) abort
    let l:executable = ale_linters#javascript#standard#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_standard_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --stdin %s'
endfunction

" standard uses eslint and the output format is the same
call ale#linter#Define('javascript', {
\   'name': 'standard',
\   'executable': function('ale_linters#javascript#standard#GetExecutable'),
\   'command': function('ale_linters#javascript#standard#GetCommand'),
\   'callback': 'ale#handlers#eslint#Handle',
\})
