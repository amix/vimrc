" Author: Horacio Sanson - https://github.com/hsanson
" Description: Support for bibclean fixer for BibTeX files.

call ale#Set('bib_bibclean_executable', 'bibclean')
call ale#Set('bib_bibclean_options', '-align-equals')

function! ale#fixers#bibclean#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'bib_bibclean_options')
    let l:executable = ale#Var(a:buffer, 'bib_bibclean_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' ' . (empty(l:options) ? '' : l:options),
    \}
endfunction
