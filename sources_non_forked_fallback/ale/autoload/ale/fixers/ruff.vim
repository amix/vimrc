" Author: Yining <zhang.yining@gmail.com>
" Description: ruff as ALE fixer for python files

function! ale#fixers#ruff#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_ruff_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return ''
endfunction

function! ale#fixers#ruff#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_ruff_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_ruff_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_ruff', ['ruff'])
endfunction

function! ale#fixers#ruff#GetCommand(buffer, version) abort
    let l:executable = ale_linters#python#ruff#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run ruff'
    \   : ''

    " NOTE: ruff version `0.0.72` implement `--fix` with stdin
    return ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_ruff_options'))
    \   . ' --fix'
    \   .  (ale#semver#GTE(a:version, [0, 0, 72]) ? ' -' : ' %s')
endfunction

function! ale#fixers#ruff#Fix(buffer) abort
    let l:fix_cmd = {buffer -> ale#semver#RunWithVersionCheck(
    \     buffer,
    \     ale#fixers#ruff#GetExecutable(buffer),
    \     '%e --version',
    \     function('ale#fixers#ruff#GetCommand'),
    \ )}(a:buffer)

    return {
    \ 'cwd': ale#fixers#ruff#GetCwd(a:buffer),
    \ 'command': l:fix_cmd,
    \}
endfunction
