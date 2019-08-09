" Author: Justin Searls https://github.com/searls, ynonp - https://github.com/ynonp, Eddie Lebow https://github.com/elebow
" based on the ale rubocop linter
" Description: StandardRB - Ruby Style Guide, with linter & automatic code fixer

call ale#Set('ruby_standardrb_executable', 'standardrb')
call ale#Set('ruby_standardrb_options', '')

function! ale_linters#ruby#standardrb#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_standardrb_executable')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'standardrb')
    \   . ' --format json --force-exclusion '
    \   . ale#Var(a:buffer, 'ruby_standardrb_options')
    \   . ' --stdin ' . ale#Escape(expand('#' . a:buffer . ':p'))
endfunction

" standardrb is based on RuboCop so the callback is the same
call ale#linter#Define('ruby', {
\   'name': 'standardrb',
\   'executable': {b -> ale#Var(b, 'ruby_standardrb_executable')},
\   'command': function('ale_linters#ruby#standardrb#GetCommand'),
\   'callback': 'ale#ruby#HandleRubocopOutput',
\})
