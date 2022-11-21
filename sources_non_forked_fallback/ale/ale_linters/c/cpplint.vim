" Author: Justin Huang <justin.y.huang@live.com>
" Description: cpplint for c files

call ale#Set('c_cpplint_executable', 'cpplint')
call ale#Set('c_cpplint_options', '')

function! ale_linters#c#cpplint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'c_cpplint_options')

    return '%e' . ale#Pad(l:options) . ' %s'
endfunction

call ale#linter#Define('c', {
\   'name': 'cpplint',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'c_cpplint_executable')},
\   'command': function('ale_linters#c#cpplint#GetCommand'),
\   'callback': 'ale#handlers#cpplint#HandleCppLintFormat',
\   'lint_file': 1,
\})
