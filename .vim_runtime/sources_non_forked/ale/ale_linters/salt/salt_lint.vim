" Author: Benjamin BINIER <poulpatine@gmail.com>
" Description: salt-lint, saltstack linter

call ale#Set('salt_salt_lint_executable', 'salt-lint')
call ale#Set('salt_salt_lint_options', '')

function! ale_linters#salt#salt_lint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'salt_salt_lint_options'))
    \   . ' --json'
endfunction

function! ale_linters#salt#salt_lint#Handle(buffer, lines) abort
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        call add(l:output, {
        \   'lnum': l:error.linenumber + 0,
        \   'code': l:error.id + 0,
        \   'text': l:error.message,
        \   'type': l:error.severity is# 'HIGH' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('salt', {
\   'name': 'salt_lint',
\   'aliases': ['salt-lint'],
\   'executable': {b -> ale#Var(b, 'salt_salt_lint_executable')},
\   'command': function('ale_linters#salt#salt_lint#GetCommand'),
\   'callback': 'ale_linters#salt#salt_lint#Handle'
\})
