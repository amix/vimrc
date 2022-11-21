" Author: https://github.com/Spixmaster
" Description: Format CSS using css-beautify from js-beautify.

call ale#Set('css_css_beautify_executable', 'css-beautify')
call ale#Set('css_css_beautify_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('css_css_beautify_options', '')

function! ale#fixers#css_beautify#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'css_css_beautify',
    \   ['css-beautify']
    \)

    let l:options = ale#Var(a:buffer, 'css_css_beautify_options')

    return {
    \   'command': ale#Escape(l:executable) . ' ' . l:options . ' -',
    \}
endfunction
