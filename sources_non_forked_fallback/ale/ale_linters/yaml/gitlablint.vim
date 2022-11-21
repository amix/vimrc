call ale#Set('yaml_gitlablint_executable', 'gll')
call ale#Set('yaml_gitlablint_options', '')

function! ale_linters#yaml#gitlablint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'yaml_gitlablint_options'))
    \   . ' -p %t'
endfunction

function! ale_linters#yaml#gitlablint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    " (<unknown>): mapping values are not allowed in this context at line 68 column 8
    " jobs:build:dev config contains unknown keys: ony
    let l:pattern = '^\(.*\) at line \(\d\+\) column \(\d\+\)$'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if !empty(l:match)
            let l:item = {
            \   'lnum': l:match[2] + 0,
            \   'col': l:match[3] + 0,
            \   'text': l:match[1],
            \   'type': 'E',
            \}
            call add(l:output, l:item)
        else
            if l:line isnot# 'GitLab CI configuration is invalid'
                let l:item = {
                \   'lnum': 0,
                \   'col': 0,
                \   'text': l:line,
                \   'type': 'E',
                \}
                call add(l:output, l:item)
            endif
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('yaml', {
\   'name': 'gitlablint',
\   'executable': {b -> ale#Var(b, 'yaml_gitlablint_executable')},
\   'command': function('ale_linters#yaml#gitlablint#GetCommand'),
\   'callback': 'ale_linters#yaml#gitlablint#Handle',
\   'output_stream': 'stderr',
\})
