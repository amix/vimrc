" Author: Axel Clark <axelclark@pm.me>
" Description: Lexical integration (https://github.com/lexical-lsp/lexical)

call ale#Set('elixir_lexical_release', 'lexical')

function! ale_linters#elixir#lexical#GetExecutable(buffer) abort
    let l:dir = ale#path#Simplify(ale#Var(a:buffer, 'elixir_lexical_release'))
    let l:cmd = has('win32') ? '\start_lexical.bat' : '/start_lexical.sh'

    return l:dir . l:cmd
endfunction

call ale#linter#Define('elixir', {
\   'name': 'lexical',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#elixir#lexical#GetExecutable'),
\   'command': function('ale_linters#elixir#lexical#GetExecutable'),
\   'project_root': function('ale#handlers#elixir#FindMixUmbrellaRoot'),
\})
