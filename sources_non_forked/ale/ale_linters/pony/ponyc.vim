" Description: ponyc linter for pony files

call ale#Set('pony_ponyc_executable', 'ponyc')
call ale#Set('pony_ponyc_options', '--pass paint')

function! ale_linters#pony#ponyc#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'pony_ponyc_options'))
endfunction

call ale#linter#Define('pony', {
\   'name': 'ponyc',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'pony_ponyc_executable')},
\   'command': function('ale_linters#pony#ponyc#GetCommand'),
\   'callback': 'ale#handlers#pony#HandlePonycFormat',
\})
