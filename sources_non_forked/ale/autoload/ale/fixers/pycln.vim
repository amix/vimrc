" Author: Yining <zhang.yining@gmail.com>
" Description: pycln as ALE fixer for python files

call ale#Set('python_pycln_executable', 'pycln')
call ale#Set('python_pycln_options', '')
call ale#Set('python_pycln_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pycln_change_directory', 1)
call ale#Set('python_pycln_auto_pipenv', 0)
call ale#Set('python_pycln_auto_poetry', 0)
call ale#Set('python_pycln_config_file', '')

function! ale#fixers#pycln#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_pycln_change_directory')
        " Run from project root if found, else from buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return '%s:h'
endfunction

function! ale#fixers#pycln#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pycln_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pycln_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pycln', ['pycln'])
endfunction

function! ale#fixers#pycln#GetCommand(buffer) abort
    let l:executable = ale#fixers#pycln#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run pycln'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
endfunction

function! ale#fixers#pycln#FixForVersion(buffer, version) abort
    let l:executable = ale#fixers#pycln#GetExecutable(a:buffer)
    let l:cmd = [ale#Escape(l:executable)]

    if l:executable =~? 'pipenv\|poetry$'
        call extend(l:cmd, ['run', 'pycln'])
    endif

    let l:options = ale#Var(a:buffer, 'python_pycln_options')

    if !empty(l:options)
        call add(l:cmd, l:options)
    endif

    let l:config_file = ale#Var(a:buffer, 'python_pycln_config_file')
    let l:config_file = l:options !~# '\v(^| )--config ' && !empty(l:config_file)
    \   ? ale#Escape(ale#path#Simplify(l:config_file))
    \   : ''

    if !empty(l:config_file)
        call add(l:cmd, '--config ' . l:config_file)
    endif

    call add(l:cmd, '--silence')

    " NOTE: pycln version `1.3.0` support reading from stdin
    call add(l:cmd, ale#semver#GTE(a:version, [1, 3, 0]) ? '-' : '%s')

    return {
    \   'cwd': ale#fixers#pycln#GetCwd(a:buffer),
    \   'command': join(l:cmd, ' '),
    \}
endfunction

function! ale#fixers#pycln#Fix(buffer) abort
    let l:executable = ale#fixers#pycln#GetExecutable(a:buffer)
    let l:command = ale#fixers#pycln#GetCommand(a:buffer) . ale#Pad('--version')

    return ale#semver#RunWithVersionCheck(
    \     a:buffer,
    \     l:executable,
    \     l:command,
    \     function('ale#fixers#pycln#FixForVersion'),
    \)
endfunction
