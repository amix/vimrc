" Author: Arash Mousavi <arash-m>
" Description: Support for ERB::Formetter https://github.com/nebulab/erb-formatter

call ale#Set('eruby_erbformatter_executable', 'erb-formatter')

function! ale#fixers#erbformatter#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'eruby_erbformatter_executable')

    return {
    \   'command': ale#Escape(l:executable) . ' -w %t',
    \   'read_temporary_file': 1,
    \}
endfunction
