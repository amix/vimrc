call ale#Set('sql_sqlfmt_executable', 'sqlfmt')
call ale#Set('sql_sqlfmt_options', '')

function! ale#fixers#sqlfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'sql_sqlfmt_executable')
    let l:options = ale#Var(a:buffer, 'sql_sqlfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -w'
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
