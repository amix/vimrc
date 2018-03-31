" Author: Sam Howie <samhowie@gmail.com>
" Description: Integration of hackfmt with ALE.

call ale#Set('php_hackfmt_executable', 'hackfmt')
call ale#Set('php_hackfmt_options', '')

function! ale#fixers#hackfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'php_hackfmt_executable')
    let l:options = ale#Var(a:buffer, 'php_hackfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -i'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
