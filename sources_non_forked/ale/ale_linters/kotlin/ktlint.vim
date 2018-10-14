" Author: Francis Agyapong <francisagyapong2@gmail.com>
" Description: Lint kotlin files using ktlint

call ale#Set('kotlin_ktlint_executable', 'ktlint')
call ale#Set('kotlin_ktlint_rulesets', [])
call ale#Set('kotlin_ktlint_format', 0)


function! ale_linters#kotlin#ktlint#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'kotlin_ktlint_executable')
    let l:file_path = expand('#' . a:buffer . ':p')
    let l:options = ''

    " Formmatted content written to original file, not sure how to handle
    " if ale#Var(a:buffer, 'kotlin_ktlint_format')
    "     let l:options = l:options . ' --format'
    " endif

    for l:ruleset in ale#Var(a:buffer, 'kotlin_ktlint_rulesets')
        let l:options = l:options . ' --ruleset ' . l:ruleset
    endfor

    return l:executable . ' ' . l:options . ' ' . l:file_path
endfunction

function! ale_linters#kotlin#ktlint#Handle(buffer, lines) abort
    let l:message_pattern = '^\(.*\):\([0-9]\+\):\([0-9]\+\):\s\+\(.*\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:message_pattern)
        let l:line = l:match[2] + 0
        let l:column = l:match[3] + 0
        let l:text = l:match[4]

        let l:type = l:text =~? 'not a valid kotlin file' ? 'E' : 'W'

        call add(l:output, {
        \   'lnum': l:line,
        \   'col': l:column,
        \   'text': l:text,
        \   'type': l:type
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('kotlin', {
\   'name': 'ktlint',
\   'executable': 'ktlint',
\   'command_callback': 'ale_linters#kotlin#ktlint#GetCommand',
\   'callback': 'ale_linters#kotlin#ktlint#Handle',
\   'lint_file': 1
\})
