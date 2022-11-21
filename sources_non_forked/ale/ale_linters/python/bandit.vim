" Author: Martino Pilia <martino.pilia@gmail.com>
" Description: bandit linting for python files

call ale#Set('python_bandit_executable', 'bandit')
call ale#Set('python_bandit_options', '')
call ale#Set('python_bandit_use_config', 1)
call ale#Set('python_bandit_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_bandit_auto_pipenv', 0)
call ale#Set('python_bandit_auto_poetry', 0)

function! ale_linters#python#bandit#GetExecutable(buffer) abort
    if (
    \   ale#Var(a:buffer, 'python_auto_pipenv')
    \   || ale#Var(a:buffer, 'python_bandit_auto_pipenv')
    \) && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (
    \   ale#Var(a:buffer, 'python_auto_poetry')
    \   || ale#Var(a:buffer, 'python_bandit_auto_poetry')
    \) && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_bandit', ['bandit'])
endfunction

function! ale_linters#python#bandit#GetCommand(buffer) abort
    let l:executable = ale_linters#python#bandit#GetExecutable(a:buffer)
    let l:flags = ' --format custom'
    \   . ' --msg-template "{line}:{test_id}:{severity}:{msg}" '

    if ale#Var(a:buffer, 'python_bandit_use_config')
        let l:config_path = ale#path#FindNearestFile(a:buffer, '.bandit')

        if !empty(l:config_path)
            let l:flags = ' --ini ' . ale#Escape(l:config_path) . l:flags
        endif
    endif

    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run bandit'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
    \   . l:flags
    \   . ale#Pad(ale#Var(a:buffer, 'python_bandit_options'))
    \   . ' -'
endfunction

function! ale_linters#python#bandit#Handle(buffer, lines) abort
    " Custom format defined in GetCommand via --msg-template
    let l:pattern = '\v^([0-9]+):(B[0-9]+):([A-Z]+):(.*)$'
    let l:severity = {'LOW': 'I', 'MEDIUM': 'W', 'HIGH': 'E'}
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': str2nr(l:match[1]),
        \   'code': l:match[2],
        \   'type': l:severity[l:match[3]],
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'bandit',
\   'executable': function('ale_linters#python#bandit#GetExecutable'),
\   'command': function('ale_linters#python#bandit#GetCommand'),
\   'callback': 'ale_linters#python#bandit#Handle',
\})
