" Author: Horacio Sanson <https://github.com/hsanson>
" Description: Support ansible language server https://github.com/ansible/ansible-language-server/

call ale#Set('ansible_language_server_executable', 'ansible-language-server')
call ale#Set('ansible_language_server_config', {})

function! ale_linters#ansible#ansible_language_server#Executable(buffer) abort
    return ale#Var(a:buffer, 'ansible_language_server_executable')
endfunction

function! ale_linters#ansible#ansible_language_server#GetCommand(buffer) abort
    let l:executable = ale_linters#ansible#ansible_language_server#Executable(a:buffer)

    return ale#Escape(l:executable) . ' --stdio'
endfunction

function! ale_linters#ansible#ansible_language_server#FindProjectRoot(buffer) abort
    let l:dir = fnamemodify(
    \   ale#path#FindNearestFile(a:buffer, 'ansible.cfg'),
    \   ':h'
    \)

    if l:dir isnot# '.' && isdirectory(l:dir)
        return l:dir
    endif

    let l:dir = fnamemodify(
    \   ale#path#FindNearestDirectory(a:buffer, '.git'),
    \   ':h:h'
    \)

    if l:dir isnot# '.' && isdirectory(l:dir)
        return l:dir
    endif

    return ''
endfunction

call ale#linter#Define('ansible', {
\   'name': 'ansible-language-server',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#ansible#ansible_language_server#Executable'),
\   'command': function('ale_linters#ansible#ansible_language_server#GetCommand'),
\   'project_root': function('ale_linters#ansible#ansible_language_server#FindProjectRoot'),
\   'lsp_config': {b -> ale#Var(b, 'ansible_language_server_config')}
\})
