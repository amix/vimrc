" Author: kmarc <korondi.mark@gmail.com>
" Description: This file adds support for using GNU awk with scripts.

call ale#Set('awk_gawk_executable', 'gawk')
call ale#Set('awk_gawk_options', '')

function! ale_linters#awk#gawk#GetCommand(buffer) abort
    " note the --source 'BEGIN ...' is to prevent
    " gawk from attempting to execute the body of the script
    " it is linting.
    return '%e --source ' . ale#Escape('BEGIN { exit } END { exit 1 }')
    \   . ' --lint'
    \   .  ale#Pad(ale#Var(a:buffer, 'awk_gawk_options'))
    \   . ' -f %t /dev/null'
endfunction

call ale#linter#Define('awk', {
\   'name': 'gawk',
\   'executable': {b -> ale#Var(b, 'awk_gawk_executable')},
\   'command': function('ale_linters#awk#gawk#GetCommand'),
\   'callback': 'ale#handlers#gawk#HandleGawkFormat',
\   'output_stream': 'both'
\})
