" Author: Jerko Steiner <https://github.com/jeremija>
" Description: https://github.com/saibing/bingo

call ale#Set('go_bingo_executable', 'bingo')
call ale#Set('go_bingo_options', '--mode stdio')

function! ale_linters#go#bingo#GetCommand(buffer) abort
    return ale#go#EnvString(a:buffer) . '%e' . ale#Pad(ale#Var(a:buffer, 'go_bingo_options'))
endfunction

function! ale_linters#go#bingo#FindProjectRoot(buffer) abort
    let l:go_modules_off = ale#Var(a:buffer, 'go_go111module') is# 'off'
    let l:project_root = l:go_modules_off ?
    \ '' : ale#path#FindNearestFile(a:buffer, 'go.mod')
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
\   'executable': {b -> ale#Var(b, 'go_bingo_executable')},
\   'command': function('ale_linters#go#bingo#GetCommand'),
\   'project_root': function('ale_linters#go#bingo#FindProjectRoot'),
\})
