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
\   'callback': 'ale#ruby#HandleRubocopOutput',
\})
