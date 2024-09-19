" Author: Arash Mousavi <arash-m>
" Description: Support for HTML Beautifier https://github.com/threedaymonk/htmlbeautifier

call ale#Set('eruby_htmlbeautifier_executable', 'htmlbeautifier')

function! ale#fixers#htmlbeautifier#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'eruby_htmlbeautifier_executable')

    return {
    \   'command': ale#Escape(l:executable) . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
