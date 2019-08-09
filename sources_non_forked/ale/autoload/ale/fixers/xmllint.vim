" Author: Cyril Roelandt <tipecaml@gmail.com>
" Description: Integration of xmllint with ALE.

call ale#Set('xml_xmllint_executable', 'xmllint')
call ale#Set('xml_xmllint_options', '')
call ale#Set('xml_xmllint_indentsize', 2)

function! ale#fixers#xmllint#Fix(buffer) abort
    let l:executable = ale#Escape(ale#Var(a:buffer, 'xml_xmllint_executable'))
    let l:filename = ale#Escape(bufname(a:buffer))
    let l:command = l:executable . ' --format ' . l:filename

    let l:indent = ale#Var(a:buffer, 'xml_xmllint_indentsize')

    if l:indent isnot# ''
        let l:env = ale#Env('XMLLINT_INDENT', repeat(' ', l:indent))
        let l:command = l:env . l:command
    endif

    let l:options = ale#Var(a:buffer, 'xml_xmllint_options')

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return {
    \   'command': l:command
    \}
endfunction
