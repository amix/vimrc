" Author: David Buchan-Swanson <github@deecewan.com>
" Description: Integrate ALE with reason-language-server.

call ale#Set('reason_ls_executable', '')

function! ale_linters#reason#ls#FindProjectRoot(buffer) abort
    let l:reason_config = ale#path#FindNearestFile(a:buffer, 'bsconfig.json')

    if !empty(l:reason_config)
        return fnamemodify(l:reason_config, ':h')
    endif

    return ''
endfunction

call ale#linter#Define('reason', {
\   'name': 'reason-language-server',
\   'aliases': ['reason_ls'],
\   'lsp': 'stdio',
\   'executable': {buffer -> ale#Var(buffer, 'reason_ls_executable')},
\   'command': '%e',
\   'project_root': function('ale_linters#reason#ls#FindProjectRoot'),
\   'language': 'reason',
\})
