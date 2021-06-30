" Author: Derek P Sifford <dereksifford@gmail.com>
" Description: Fixer for C, C++, C#, ObjectiveC, D, Java, Pawn, and VALA.

call ale#Set('c_uncrustify_executable', 'uncrustify')
call ale#Set('c_uncrustify_options', '')

function! ale#fixers#uncrustify#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'c_uncrustify_executable')
    let l:options = ale#Var(a:buffer, 'c_uncrustify_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' --no-backup'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction
