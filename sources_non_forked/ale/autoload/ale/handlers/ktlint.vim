" Author: Michael Phillips <michaeljoelphillips@gmail.com>
" Description: Handler functions for ktlint.

call ale#Set('kotlin_ktlint_executable', 'ktlint')
call ale#Set('kotlin_ktlint_rulesets', [])
call ale#Set('kotlin_ktlint_options', '')

function! ale#handlers#ktlint#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'kotlin_ktlint_executable')
    let l:options = ale#Var(a:buffer, 'kotlin_ktlint_options')
    let l:rulesets = ale#handlers#ktlint#GetRulesets(a:buffer)

    return ale#Escape(l:executable)
    \   . (empty(l:options) ? '' : ' ' . l:options)
    \   . (empty(l:rulesets) ? '' : ' ' . l:rulesets)
    \   . ' --stdin'
endfunction

function! ale#handlers#ktlint#GetRulesets(buffer) abort
    let l:rulesets = map(ale#Var(a:buffer, 'kotlin_ktlint_rulesets'), '''--ruleset '' . v:val')

    return join(l:rulesets, ' ')
endfunction

function! ale#handlers#ktlint#Handle(buffer, lines) abort
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
