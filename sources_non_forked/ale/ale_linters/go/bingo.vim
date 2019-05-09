" Author: Jerko Steiner <https://github.com/jeremija>
" Description: https://github.com/saibing/bingo

call ale#Set('go_bingo_executable', 'bingo')
call ale#Set('go_bingo_options', '--mode stdio')

function! ale_linters#go#bingo#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'go_bingo_options'))
endfunction

function! ale_linters#go#bingo#FindProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'go.mod')
    let l:mods = ':h'

    if empty(l:project_root)
        let l:project_root = ale#path#FindNearestDirectory(a:buffer, '.git')
        let l:mods = ':h:h'
    endif

    return !empty(l:project_root) ? fnamemodify(l:project_root, l:mods) : ''
endfunction

call ale#linter#Define('go', {
\   'name': 'bingo',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('go_bingo_executable'),
\   'command_callback': 'ale_linters#go#bingo#GetCommand',
\   'project_root_callback': 'ale_linters#go#bingo#FindProjectRoot',
\})
