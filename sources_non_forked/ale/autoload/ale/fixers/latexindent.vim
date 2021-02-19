" Author: riley-martine <riley.martine@protonmail.com>
" Description: Integration of latexindent with ALE.

call ale#Set('tex_latexindent_executable', 'latexindent')
call ale#Set('tex_latexindent_options', '')

function! ale#fixers#latexindent#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'tex_latexindent_executable')
    let l:options = ale#Var(a:buffer, 'tex_latexindent_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -l'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction
