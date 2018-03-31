" Description: ponyc linter for pony files

call ale#Set('pony_ponyc_executable', 'ponyc')
call ale#Set('pony_ponyc_options', '--pass paint')

function! ale_linters#pony#ponyc#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'pony_ponyc_executable')
endfunction

function! ale_linters#pony#ponyc#GetCommand(buffer) abort
    return ale#Escape(ale_linters#pony#ponyc#GetExecutable(a:buffer))
    \   . ' ' . ale#Var(a:buffer, 'pony_ponyc_options')
endfunction

call ale#linter#Define('pony', {
\   'name': 'ponyc',
\   'output_stream': 'stderr',
\   'executable_callback': 'ale_linters#pony#ponyc#GetExecutable',
\   'command_callback': 'ale_linters#pony#ponyc#GetCommand',
\   'callback': 'ale#handlers#pony#HandlePonycFormat',
\})
