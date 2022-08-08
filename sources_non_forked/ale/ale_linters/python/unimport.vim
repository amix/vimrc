" Author: Author: Jon Parise <jon@indelible.org>

call ale#Set('python_unimport_executable', 'unimport')
call ale#Set('python_unimport_options', '')
call ale#Set('python_unimport_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_unimport_auto_pipenv', 0)
call ale#Set('python_unimport_auto_poetry', 0)

function! ale_linters#python#unimport#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_unimport_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_unimport_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_unimport', ['unimport'])
endfunction

function! ale_linters#python#unimport#GetCommand(buffer) abort
    let l:executable = ale_linters#python#unimport#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run unimport'
    \   : ''

    return '%e' . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_unimport_options'))
    \   . ' --check'
    \   . ' %t'
endfunction


function! ale_linters#python#unimport#GetCwd(buffer) abort
    let l:project_root = ale#python#FindProjectRoot(a:buffer)

    return !empty(l:project_root)
    \   ? l:project_root
    \   : expand('#' . a:buffer . ':p:h')
endfunction


function! ale_linters#python#unimport#Handle(buffer, lines) abort
    let l:output = ale#python#HandleTraceback(a:lines, 10)

    if !empty(l:output)
        return l:output
    endif

    " Matches lines like:
    "
    " urllib.parse at path/to/file.py:9
    let l:pattern = '\v(.+) at [^:]+:(\d+)$'

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'type': 'W',
        \   'text': 'unused: ' . l:match[1],
        \})
    endfor

    return l:output
endfunction


call ale#linter#Define('python', {
\   'name': 'unimport',
\   'executable': function('ale_linters#python#unimport#GetExecutable'),
\   'cwd': function('ale_linters#python#unimport#GetCwd'),
\   'command': function('ale_linters#python#unimport#GetCommand'),
\   'callback': 'ale_linters#python#unimport#Handle',
\})
