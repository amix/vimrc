" Author: WhyNotHugo <hugo@barrera.io>
" Description: Format HTML files with html-beautify.

call ale#Set('html_beautify_executable', 'html-beautify')
call ale#Set('html_beautify_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('html_beautify_options', '')

function! ale#fixers#html_beautify#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'html_beautify',
    \   ['html-beautify']
    \)

    let l:options = ale#Var(a:buffer, 'html_beautify_options')

    return {
    \   'command': ale#Escape(l:executable) . ' ' . l:options . ' -',
    \}
endfunction
