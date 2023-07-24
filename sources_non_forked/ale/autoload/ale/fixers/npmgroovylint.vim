" Author: lucas-str <lucas.sturelle@ik.me>
" Description: Integration of npm-groovy-lint for Groovy files.

call ale#Set('groovy_npmgroovylint_fix_options', '--fix')

function! ale#fixers#npmgroovylint#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'groovy_npmgroovylint_executable')
    let l:options = ale#Var(a:buffer, 'groovy_npmgroovylint_fix_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
