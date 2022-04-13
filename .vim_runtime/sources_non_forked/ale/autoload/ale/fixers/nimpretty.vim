" Author: Nhan <hi@imnhan.com>
" Description: Integration of nimpretty with ALE.

call ale#Set('nim_nimpretty_executable', 'nimpretty')
call ale#Set('nim_nimpretty_options', '--maxLineLen:80')

function! ale#fixers#nimpretty#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nim_nimpretty_executable')
    let l:options = ale#Var(a:buffer, 'nim_nimpretty_options')

    return {
    \   'command': ale#Escape(l:executable) . ' %t' . ale#Pad(l:options),
    \   'read_temporary_file': 1,
    \}
endfunction
