call ale#Set('hurl_hurlfmt_executable', 'hurlfmt')

function! ale#fixers#hurlfmt#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'hurl_hurlfmt_executable')

    return ale#Escape(l:executable)
    \   . ' --out hurl'
endfunction

function! ale#fixers#hurlfmt#Fix(buffer) abort
    return {
    \   'command': ale#fixers#hurlfmt#GetCommand(a:buffer)
    \}
endfunction

