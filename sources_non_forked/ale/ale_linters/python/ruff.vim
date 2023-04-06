" Author: Yining <zhang.yining@gmail.com>
" Description: ruff as linter for python files

call ale#Set('python_ruff_executable', 'ruff')
call ale#Set('python_ruff_options', '')
call ale#Set('python_ruff_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_ruff_change_directory', 1)
call ale#Set('python_ruff_auto_pipenv', 0)
call ale#Set('python_ruff_auto_poetry', 0)

call ale#fix#registry#Add('ruff',
\   'ale#fixers#ruff#Fix',
\   ['python'],
\   'A python linter/fixer for Python written in Rust'
\)

function! ale_linters#python#ruff#GetExecutable(buffer) abort
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

function! ale_linters#python#ruff#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_ruff_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return ''
endfunction

function! ale_linters#python#ruff#GetCommand(buffer, version) abort
    let l:executable = ale_linters#python#ruff#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run ruff'
    \   : ''

    " NOTE: ruff version `0.0.69` supports liniting input from stdin
    return ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_ruff_options'))
    \   . ' --format text'
    \   .  (ale#semver#GTE(a:version, [0, 0, 69]) ? ' --stdin-filename %s -' : ' %s')
endfunction

function! ale_linters#python#ruff#Handle(buffer, lines) abort
    "Example: path/to/file.py:10:5: E999 SyntaxError: unexpected indent
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+)?:? (.+)$'
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
\   'name': 'ruff',
\   'executable': function('ale_linters#python#ruff#GetExecutable'),
\   'cwd': function('ale_linters#python#ruff#GetCwd'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#python#ruff#GetExecutable(buffer),
\       '%e --version',
\       function('ale_linters#python#ruff#GetCommand'),
\   )},
\   'callback': 'ale_linters#python#ruff#Handle',
\   'output_stream': 'both',
\   'read_buffer': 1,
\})
