" Author: Cluas <Cluas@live.cn>
" Description: Fixing files with sqlformat.

call ale#Set('sql_sqlformat_executable', 'sqlformat')
call ale#Set('sql_sqlformat_options', '')

function! ale#fixers#sqlformat#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'sql_sqlformat_executable')
    let l:options = ale#Var(a:buffer, 'sql_sqlformat_options')

    return {
    \ 'command': ale#Escape(l:executable)
    \ . (!empty(l:options) ? ' ' . l:options : '')
    \ . ' -'
    \}
endfunction
