" Author: Peter Benjamin <petermbenjamin@gmail.com>
" Description: Linter for GitHub Workflows

call ale#Set('yaml_actionlint_executable', 'actionlint')
call ale#Set('yaml_actionlint_options', '')

function! ale_linters#yaml#actionlint#GetCommand(buffer) abort
    " Only execute actionlint on YAML files in /.github/ paths.
    if expand('#' . a:buffer . ':p') !~# '\v[/\\]\.github[/\\]'
        return ''
    endif

    let l:options = ale#Var(a:buffer, 'yaml_actionlint_options')

    if l:options !~# '-no-color'
        let l:options .= ale#Pad('-no-color')
    endif

    if l:options !~# '-oneline'
        let l:options .= ale#Pad('-oneline')
    endif

    return '%e' . ale#Pad(l:options) . ' - '
endfunction

function! ale_linters#yaml#actionlint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    ".github/workflows/main.yml:19:0: could not parse as YAML: yaml: line 19: mapping values are not allowed in this context [yaml-syntax]
    let l:pattern = '\v^.{-}:(\d+):(\d+): (.+) \[(.+)\]$'
    let l:output = []


    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[4]
        let l:text = l:match[3]

        " Handle sub-linter errors like the following:
        "validate.yml:19:9: shellcheck reported issue in this script: SC2086:info:1:15: Double quote to prevent globbing and word splitting [shellcheck]
        if l:code is# 'shellcheck'
            let l:shellcheck_match = matchlist(l:text, '\v^.+: (SC\d{4}):.+:\d+:\d+: (.+)$')
            let l:text = l:shellcheck_match[2]
            let l:code = 'shellcheck ' . l:shellcheck_match[1]
        endif

        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:text,
        \   'code': l:code,
        \   'type': 'E',
        \}

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('yaml', {
\   'name': 'actionlint',
\   'executable': {b -> ale#Var(b, 'yaml_actionlint_executable')},
\   'command': function('ale_linters#yaml#actionlint#GetCommand'),
\   'callback': 'ale_linters#yaml#actionlint#Handle',
\})
