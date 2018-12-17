" Author: aclemons - https://github.com/aclemons
" based on the ale rubocop linter
" Description: Ruumba, RuboCop linting for ERB templates.

call ale#Set('eruby_ruumba_executable', 'ruumba')
call ale#Set('eruby_ruumba_options', '')

function! ale_linters#eruby#ruumba#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'eruby_ruumba_executable')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'ruumba')
    \   . ' --format json --force-exclusion '
    \   . ale#Var(a:buffer, 'eruby_ruumba_options')
    \   . ' --stdin ' . ale#Escape(expand('#' . a:buffer . ':p'))
endfunction

function! ale_linters#eruby#ruumba#Handle(buffer, lines) abort
    try
        let l:errors = json_decode(a:lines[0])
    catch
        return []
    endtry

    if !has_key(l:errors, 'summary')
    \|| l:errors['summary']['offense_count'] == 0
    \|| empty(l:errors['files'])
        return []
    endif

    let l:output = []

    for l:error in l:errors['files'][0]['offenses']
        let l:start_col = l:error['location']['column'] + 0
        call add(l:output, {
        \   'lnum': l:error['location']['line'] + 0,
        \   'col': l:start_col,
        \   'end_col': l:start_col + l:error['location']['length'] - 1,
        \   'code': l:error['cop_name'],
        \   'text': l:error['message'],
        \   'type': ale_linters#eruby#ruumba#GetType(l:error['severity']),
        \})
    endfor

    return l:output
endfunction

function! ale_linters#eruby#ruumba#GetType(severity) abort
    if a:severity is? 'convention'
    \|| a:severity is? 'warning'
    \|| a:severity is? 'refactor'
        return 'W'
    endif

    return 'E'
endfunction

call ale#linter#Define('eruby', {
\   'name': 'ruumba',
\   'executable_callback': ale#VarFunc('eruby_ruumba_executable'),
\   'command_callback': 'ale_linters#eruby#ruumba#GetCommand',
\   'callback': 'ale_linters#eruby#ruumba#Handle',
\})
