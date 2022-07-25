" Author: Drew Olson <drew@drewolson.org>
" Description: Integrate ALE with purescript-language-server.

call ale#Set('purescript_ls_executable', 'purescript-language-server')
call ale#Set('purescript_ls_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('purescript_ls_config', {})

function! ale_linters#purescript#ls#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'purescript_ls', [
    \   'node_modules/.bin/purescript-language-server',
    \])
endfunction

function! ale_linters#purescript#ls#GetCommand(buffer) abort
    let l:executable = ale_linters#purescript#ls#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' --stdio'
endfunction

function! ale_linters#purescript#ls#FindProjectRoot(buffer) abort
    let l:config = ale#path#FindNearestFile(a:buffer, 'bower.json')

    if !empty(l:config)
        return fnamemodify(l:config, ':h')
    endif

    let l:config = ale#path#FindNearestFile(a:buffer, 'psc-package.json')

    if !empty(l:config)
        return fnamemodify(l:config, ':h')
    endif

    let l:config = ale#path#FindNearestFile(a:buffer, 'spago.dhall')

    if !empty(l:config)
        return fnamemodify(l:config, ':h')
    endif

    return ''
endfunction

call ale#linter#Define('purescript', {
\   'name': 'purescript-language-server',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#purescript#ls#GetExecutable'),
\   'command': function('ale_linters#purescript#ls#GetCommand'),
\   'project_root': function('ale_linters#purescript#ls#FindProjectRoot'),
\   'lsp_config': {b -> ale#Var(b, 'purescript_ls_config')},
\})
