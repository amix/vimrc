" Author: Ahmed El Gabri <@ahmedelgabri>
" Description: standardjs for typescript files

call ale#Set('typescript_standard_executable', 'standard')
call ale#Set('typescript_standard_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('typescript_standard_options', '')

function! ale_linters#typescript#standard#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'typescript_standard', [
    \   'node_modules/standardx/bin/cmd.js',
    \   'node_modules/standard/bin/cmd.js',
    \   'node_modules/.bin/standard',
    \])
endfunction

function! ale_linters#typescript#standard#GetCommand(buffer) abort
    let l:executable = ale_linters#typescript#standard#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'typescript_standard_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --stdin %s'
endfunction

" standard uses eslint and the output format is the same
call ale#linter#Define('typescript', {
\   'name': 'standard',
\   'executable': function('ale_linters#typescript#standard#GetExecutable'),
\   'command': function('ale_linters#typescript#standard#GetCommand'),
\   'callback': 'ale#handlers#eslint#Handle',
\})
