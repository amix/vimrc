" Author: Yining <zhang.yining@gmail.com>
" Description: refurb as linter for python files

call ale#Set('python_refurb_executable', 'refurb')
call ale#Set('python_refurb_options', '')
call ale#Set('python_refurb_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_refurb_change_directory', 1)
call ale#Set('python_refurb_auto_pipenv', 0)
call ale#Set('python_refurb_auto_poetry', 0)
call ale#Set('python_refurb_auto_uv', 0)

function! ale_linters#python#refurb#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_refurb_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_refurb_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_refurb_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_refurb', ['refurb'])
endfunction

function! ale_linters#python#refurb#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_refurb_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return ''
endfunction

function! ale_linters#python#refurb#GetCommand(buffer) abort
    let l:executable = ale_linters#python#refurb#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run refurb'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_refurb_options'))
    \   . ' %s'
endfunction

function! ale_linters#python#refurb#Handle(buffer, lines) abort
    "Example: path/to/file.py:3:17 [FURB109]: Replace `in [x, y, z]` with `in (x, y, z)`
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+)?:?\s*\[FURB(\d+)\]:\s*(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'code': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'refurb',
\   'executable': function('ale_linters#python#refurb#GetExecutable'),
\   'cwd': function('ale_linters#python#refurb#GetCwd'),
\   'command':  function('ale_linters#python#refurb#GetCommand'),
\   'callback': 'ale_linters#python#refurb#Handle',
\   'output_stream': 'both',
\   'read_buffer': 0,
\})
