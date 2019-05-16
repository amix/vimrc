" Author: w0rp <devw0rp@gmail.com>
" Author: Jerko Steiner <https://github.com/jeremija>
" Description: https://github.com/saibing/gopls

call ale#Set('go_gopls_executable', 'gopls')
call ale#Set('go_gopls_options', '--mode stdio')

function! ale_linters#go#gopls#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'go_gopls_options'))
endfunction

function! ale_linters#go#gopls#FindProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'go.mod')
    let l:mods = ':h'

    if empty(l:project_root)
        let l:project_root = ale#path#FindNearestDirectory(a:buffer, '.git')
        let l:mods = ':h:h'
    endif

    return !empty(l:project_root) ? fnamemodify(l:project_root, l:mods) : ''
endfunction

call ale#linter#Define('go', {
\   'name': 'gopls',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'go_gopls_executable')},
\   'command': function('ale_linters#go#gopls#GetCommand'),
\   'project_root': function('ale_linters#go#gopls#FindProjectRoot'),
\})
