" Author: Robert Liebowitz <rliebz@gmail.com>
" Description: https://github.com/johnnymorganz/stylua

call ale#Set('lua_stylua_executable', 'stylua')
call ale#Set('lua_stylua_options', '')

function! ale#fixers#stylua#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'lua_stylua_executable')
    let l:options = ale#Var(a:buffer, 'lua_stylua_options')

    return {
    \   'command': ale#Escape(l:executable) . ale#Pad(l:options) . ' -',
    \}
endfunction
