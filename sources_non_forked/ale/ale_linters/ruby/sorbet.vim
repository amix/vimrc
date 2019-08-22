call ale#Set('ruby_sorbet_executable', 'srb')
call ale#Set('ruby_sorbet_options', '')

function! ale_linters#ruby#sorbet#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_sorbet_executable')
    let l:options = ale#Var(a:buffer, 'ruby_sorbet_options')

    return ale#handlers#ruby#EscapeExecutable(l:executable, 'srb')
    \   . ' tc'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --lsp --disable-watchman'
endfunction

call ale#linter#Define('ruby', {
\   'name': 'sorbet',
\   'aliases': ['srb'],
\   'lsp': 'stdio',
\   'language': 'ruby',
\   'executable': {b -> ale#Var(b, 'ruby_sorbet_executable')},
\   'command': function('ale_linters#ruby#sorbet#GetCommand'),
\   'project_root': function('ale#ruby#FindProjectRoot')
\})

