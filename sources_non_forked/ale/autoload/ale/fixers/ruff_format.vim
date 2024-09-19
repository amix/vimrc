" Author: Yining <zhang.yining@gmail.com>, Joseph Henrich <crimsonknave@gmail.com>
" Description: ruff formatter as ALE fixer for python files

call ale#Set('python_ruff_format_executable', 'ruff')
call ale#Set('python_ruff_format_options', '')
call ale#Set('python_ruff_format_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_ruff_format_change_directory', 1)
call ale#Set('python_ruff_format_auto_pipenv', 0)
call ale#Set('python_ruff_format_auto_poetry', 0)
call ale#Set('python_ruff_format_auto_uv', 0)

function! ale#fixers#ruff_format#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_ruff_format_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return '%s:h'
endfunction

function! ale#fixers#ruff_format#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_ruff_format_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_ruff_format_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_ruff_format_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_ruff_format', ['ruff'])
endfunction

function! ale#fixers#ruff_format#GetCommand(buffer) abort
    let l:executable = ale#fixers#ruff_format#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run ruff'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
endfunction

function! ale#fixers#ruff_format#Fix(buffer) abort
    let l:executable = ale#fixers#ruff_format#GetExecutable(a:buffer)
    let l:cmd = [ale#Escape(l:executable)]

    if l:executable =~? 'pipenv\|poetry\|uv$'
        call extend(l:cmd, ['run', 'ruff'])
    endif

    let l:options = ale#Var(a:buffer, 'python_ruff_format_options')

    " when --stdin-filename present, ruff will use it for proj root resolution
    " https://github.com/charliermarsh/ruff/pull/1281
    let l:fname = expand('#' . a:buffer . '...')
    call add(l:cmd, 'format')

    if !empty(l:options)
        call add(l:cmd, l:options)
    endif

    call add(l:cmd, '--stdin-filename '.ale#Escape(ale#path#Simplify(l:fname)))

    call add(l:cmd, '-')

    return {
    \   'cwd': ale#fixers#ruff_format#GetCwd(a:buffer),
    \   'command': join(l:cmd, ' '),
    \}
endfunction
