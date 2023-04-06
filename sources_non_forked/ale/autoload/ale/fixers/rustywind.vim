scriptencoding utf-8
" Author: Guillermo Roig <groig@protonmail.com>
" Description: Sort TailwindCSS classes with rustywind

call ale#Set('html_rustywind_executable', 'rustywind')
call ale#Set('html_rustywind_options', '')

function! ale#fixers#rustywind#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'html_rustywind_executable')
    let l:options = ale#Var(a:buffer, 'html_rustywind_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --stdin'
    \}
endfunction
