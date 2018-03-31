" Author: kmarc <korondi.mark@gmail.com>
" Description: This file adds support for using GNU awk with sripts.

let g:ale_awk_gawk_executable =
\   get(g:, 'ale_awk_gawk_executable', 'gawk')

let g:ale_awk_gawk_options =
\   get(g:, 'ale_awk_gawk_options', '')

function! ale_linters#awk#gawk#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'awk_gawk_executable')
endfunction

function! ale_linters#awk#gawk#GetCommand(buffer) abort
    " note the --source 'BEGIN ...' is to prevent
    " gawk from attempting to execute the body of the script
    " it is linting.
    return ale_linters#awk#gawk#GetExecutable(a:buffer)
    \   . " --source 'BEGIN { exit } END { exit 1 }'"
    \   . ' ' . ale#Var(a:buffer, 'awk_gawk_options')
    \   . ' ' . '-f %t --lint /dev/null'
endfunction

call ale#linter#Define('awk', {
\   'name': 'gawk',
\   'executable_callback': 'ale_linters#awk#gawk#GetExecutable',
\   'command_callback': 'ale_linters#awk#gawk#GetCommand',
\   'callback': 'ale#handlers#gawk#HandleGawkFormat',
\   'output_stream': 'both'
\})
