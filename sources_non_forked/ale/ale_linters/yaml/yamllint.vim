" Author: KabbAmine <amine.kabb@gmail.com>

call ale#Set('yaml_yamllint_executable', 'yamllint')
call ale#Set('yaml_yamllint_options', '')

function! ale_linters#yaml#yamllint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'yaml_yamllint_options'))
    \   . ' -f parsable %t'
endfunction

function! ale_linters#yaml#yamllint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    " something.yaml:1:1: [warning] missing document start "---" (document-start)
    " something.yml:2:1: [error] syntax error: expected the node content, but found '<stream end>'
    let l:pattern = '\v^.*:(\d+):(\d+): \[(error|warning)\] (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'type': l:match[3] is# 'error' ? 'E' : 'W',
        \}

        let l:code_match = matchlist(l:item.text, '\v^(.+) \(([^)]+)\)$')

        if !empty(l:code_match)
            if l:code_match[2] is# 'trailing-spaces'
            \&& !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
                " Skip warnings for trailing whitespace if the option is off.
                continue
            endif

            let l:item.text = l:code_match[1]
            let l:item.code = l:code_match[2]
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('yaml', {
\   'name': 'yamllint',
\   'executable': {b -> ale#Var(b, 'yaml_yamllint_executable')},
\   'command': function('ale_linters#yaml#yamllint#GetCommand'),
\   'callback': 'ale_linters#yaml#yamllint#Handle',
\})
