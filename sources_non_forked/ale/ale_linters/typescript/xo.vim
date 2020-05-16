call ale#Set('typescript_xo_executable', 'xo')
call ale#Set('typescript_xo_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('typescript_xo_options', '')

function! ale_linters#typescript#xo#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'typescript_xo', [
    \   'node_modules/.bin/xo',
    \])
endfunction

function! ale_linters#typescript#xo#GetCommand(buffer) abort
    return ale#Escape(ale_linters#typescript#xo#GetExecutable(a:buffer))
    \   . ale#Pad(ale#Var(a:buffer, 'typescript_xo_options'))
    \   . ' --reporter json --stdin --stdin-filename %s'
endfunction

" xo uses eslint and the output format is the same
call ale#linter#Define('typescript', {
\   'name': 'xo',
\   'executable': function('ale_linters#typescript#xo#GetExecutable'),
\   'command': function('ale_linters#typescript#xo#GetCommand'),
\   'callback': 'ale#handlers#eslint#HandleJSON',
\})
