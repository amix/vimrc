" Author: ynonp - https://github.com/ynonp, Eddie Lebow https://github.com/elebow
" Description: RuboCop, a code style analyzer for Ruby files

call ale#Set('ruby_rubocop_executable', 'rubocop')
call ale#Set('ruby_rubocop_options', '')

function! ale_linters#ruby#rubocop#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_rubocop_executable')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'rubocop')
    \   . ' --format json --force-exclusion '
    \   . ale#Var(a:buffer, 'ruby_rubocop_options')
    \   . ' --stdin ' . ale#Escape(expand('#' . a:buffer . ':p'))
endfunction

function! ale_linters#ruby#rubocop#Handle(buffer, lines) abort
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
        \   'type': ale_linters#ruby#rubocop#GetType(l:error['severity']),
        \})
    endfor

    return l:output
endfunction

function! ale_linters#ruby#rubocop#GetType(severity) abort
    if a:severity is? 'convention'
    \|| a:severity is? 'warning'
    \|| a:severity is? 'refactor'
        return 'W'
    endif

    return 'E'
endfunction

call ale#linter#Define('ruby', {
\   'name': 'rubocop',
\   'executable_callback': ale#VarFunc('ruby_rubocop_executable'),
\   'command_callback': 'ale_linters#ruby#rubocop#GetCommand',
\   'callback': 'ale_linters#ruby#rubocop#Handle',
\})
