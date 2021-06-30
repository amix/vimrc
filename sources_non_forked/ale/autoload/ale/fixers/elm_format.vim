" Author: soywod <clement.douin@gmail.com>
" Description: Integration of elm-format with ALE.

call ale#Set('elm_format_executable', 'elm-format')
call ale#Set('elm_format_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('elm_format_options', '--yes')

function! ale#fixers#elm_format#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'elm_format', [
    \   'node_modules/.bin/elm-format',
    \])
endfunction

function! ale#fixers#elm_format#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'elm_format_options')

    return {
    \   'command': ale#Escape(ale#fixers#elm_format#GetExecutable(a:buffer))
    \       . ' %t'
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \   'read_temporary_file': 1,
    \}
endfunction
