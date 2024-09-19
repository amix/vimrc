" Author: Yining <zhang.yining@gmail.com>
" Description: pycln as linter for python files

call ale#Set('python_pycln_executable', 'pycln')
call ale#Set('python_pycln_options', '')
call ale#Set('python_pycln_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pycln_change_directory', 1)
call ale#Set('python_pycln_auto_pipenv', 0)
call ale#Set('python_pycln_auto_poetry', 0)
call ale#Set('python_pycln_auto_uv', 0)
call ale#Set('python_pycln_config_file', '')

function! ale_linters#python#pycln#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pycln_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pycln_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_pycln_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pycln', ['pycln'])
endfunction

function! ale_linters#python#pycln#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_pycln_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return ''
endfunction

function! ale_linters#python#pycln#GetCommand(buffer, version) abort
    let l:executable = ale_linters#python#pycln#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run pycln'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_pycln_options')
    let l:config_file = ale#Var(a:buffer, 'python_pycln_config_file')
    let l:config_file = l:options !~# '\v(^| )--config ' && !empty(l:config_file)
    \   ? ale#Escape(ale#path#Simplify(l:config_file))
    \   : ''

    " NOTE: pycln version `1.3.0` supports liniting input from stdin
    return ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_pycln_options'))
    \   . (empty(l:config_file) ? '' : ' --config ' . l:config_file)
    \   . ' --check'
    \   . (ale#semver#GTE(a:version, [1, 3, 0]) ? ' -' : ' %s')
endfunction

function! ale_linters#python#pycln#Handle(buffer, lines) abort
    " Example: tmp/test.py:3:0 'import os' would be removed!
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+):? (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'pycln',
\   'executable': function('ale_linters#python#pycln#GetExecutable'),
\   'cwd': function('ale_linters#python#pycln#GetCwd'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#python#pycln#GetExecutable(buffer),
\       '%e --version',
\       function('ale_linters#python#pycln#GetCommand'),
\   )},
\   'callback': 'ale_linters#python#pycln#Handle',
\   'output_stream': 'both',
\   'read_buffer': 1,
\})
