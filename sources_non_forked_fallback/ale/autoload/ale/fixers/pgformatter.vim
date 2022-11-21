call ale#Set('sql_pgformatter_executable', 'pg_format')
call ale#Set('sql_pgformatter_options', '')

function! ale#fixers#pgformatter#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'sql_pgformatter_executable')
    let l:options = ale#Var(a:buffer, 'sql_pgformatter_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
