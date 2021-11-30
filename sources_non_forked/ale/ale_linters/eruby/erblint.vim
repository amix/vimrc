" Author: Roeland Moors - https://github.com/roelandmoors
" based on the ale ruumba and robocop linters
" Description: ERB Lint, support for https://github.com/Shopify/erb-lint

call ale#Set('eruby_erblint_executable', 'erblint')
call ale#Set('eruby_erblint_options', '')

function! ale_linters#eruby#erblint#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'eruby_erblint_executable')

    return ale#ruby#EscapeExecutable(l:executable, 'erblint')
    \   . ' --format json '
    \   . ale#Var(a:buffer, 'eruby_erblint_options')
    \   . ' --stdin %s'
endfunction

function! ale_linters#eruby#erblint#Handle(buffer, lines) abort
    if empty(a:lines)
        return []
    endif

    let l:errors = ale#util#FuzzyJSONDecode(a:lines[0], [])

    if !has_key(l:errors, 'summary')
    \|| l:errors['summary']['offenses'] == 0
    \|| empty(l:errors['files'])
        return []
    endif

    let l:output = []

    for l:error in l:errors['files'][0]['offenses']
        call add(l:output, {
        \   'lnum': l:error['location']['start_line'] + 0,
        \   'col': l:error['location']['start_column'] + 0,
        \   'end_col': l:error['location']['last_column'] + 0,
        \   'code': l:error['linter'],
        \   'text': l:error['message'],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('eruby', {
\   'name': 'erblint',
\   'executable': {b -> ale#Var(b, 'eruby_erblint_executable')},
\   'command': function('ale_linters#eruby#erblint#GetCommand'),
\   'callback': 'ale_linters#eruby#erblint#Handle',
\})
