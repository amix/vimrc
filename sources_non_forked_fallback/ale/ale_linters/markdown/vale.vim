" Author: chew-z https://github.com/chew-z
" Description: vale for Markdown files

call ale#Set('markdown_vale_executable', 'vale')
call ale#Set('markdown_vale_input_file', '%t')
call ale#Set('markdown_vale_options', '')

function! ale_linters#markdown#vale#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'markdown_vale_executable')
    let l:input_file = ale#Var(a:buffer, 'markdown_vale_input_file')

    " Defaults to `vale --output=JSON %t`
    return ale#Escape(l:executable)
    \   . ' --output=JSON '
    \   . ale#Var(a:buffer, 'markdown_vale_options')
    \   . ' ' . l:input_file
endfunction

call ale#linter#Define('markdown', {
\   'name': 'vale',
\   'executable': {b -> ale#Var(b, 'markdown_vale_executable')},
\   'command': function('ale_linters#markdown#vale#GetCommand'),
\   'callback': 'ale#handlers#vale#Handle',
\})
