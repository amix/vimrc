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
    " NOTE: ruff version `0.1.0` deprecates `--format text`
    return ale#Escape(l:executable) . l:exec_args . ' -q'
    \   . ale#Pad(ale#Var(a:buffer, 'python_ruff_options'))
    \   . (ale#semver#GTE(a:version, [0, 1, 0]) ? ' --output-format json-lines' : ' --format json-lines')
    \   . (ale#semver#GTE(a:version, [0, 0, 69]) ? ' --stdin-filename %s -' : ' %s')
endfunction

function! ale_linters#python#ruff#Handle(buffer, lines) abort
    let l:output = []

    for l:line in a:lines
        let l:item = json_decode(l:line)
        call add(l:output, {
        \   'lnum': l:item.location.row,
        \   'col': l:item.location.column,
        \   'end_lnum': l:item.end_location.row,
        \   'end_col': l:item.end_location.column - 1,
        \   'code': l:item.code,
        \   'text': l:item.message,
        \   'type': l:item.code =~? '\vE\d+' ? 'E' : 'W',
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
