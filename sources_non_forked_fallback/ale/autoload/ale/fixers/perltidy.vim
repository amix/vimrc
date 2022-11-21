" Author: kfly8 <kentafly88@gmail.com>
" Description: Integration of perltidy with ALE.

call ale#Set('perl_perltidy_executable', 'perltidy')
call ale#Set('perl_perltidy_options', '')

function! ale#fixers#perltidy#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'perl_perltidy_executable')
    let l:options = ale#Var(a:buffer, 'perl_perltidy_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -b'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
