" Author: kmarc <korondi.mark@gmail.com>
" Description: This file adds support for using GNU awk with sripts.

call ale#Set('awk_gawk_executable', 'gawk')
call ale#Set('awk_gawk_options', '')

function! ale_linters#awk#gawk#GetCommand(buffer) abort
    " note the --source 'BEGIN ...' is to prevent
    " gawk from attempting to execute the body of the script
    " it is linting.
    return '%e --source ' . ale#Escape('BEGIN { exit } END { exit 1 }')
    \   .  ale#Pad(ale#Var(a:buffer, 'awk_gawk_options'))
    \   . ' -f %t --lint /dev/null'
endfunction

call ale#linter#Define('awk', {
\   'name': 'gawk',
\   'executable_callback': ale#VarFunc('awk_gawk_executable'),
\   'command_callback': 'ale_linters#awk#gawk#GetCommand',
\   'callback': 'ale#handlers#gawk#HandleGawkFormat',
\   'output_stream': 'both'
\})
