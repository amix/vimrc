call ale#Set('lua_luafmt_executable', 'luafmt')
call ale#Set('lua_luafmt_options', '')

function! ale#fixers#luafmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'lua_luafmt_executable')
    let l:options = ale#Var(a:buffer, 'lua_luafmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --stdin',
    \}
endfunction
