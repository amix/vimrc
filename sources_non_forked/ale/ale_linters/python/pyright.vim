call ale#Set('python_pyright_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pyright_executable', 'pyright-langserver')
call ale#Set('python_pyright_config', {})
call ale#Set('python_pyright_auto_pipenv', 0)
call ale#Set('python_pyright_auto_poetry', 0)
call ale#Set('python_pyright_auto_uv', 0)

" Force the cwd of the server to be the same as the project root to
" fix issues with treating local files matching first or third party library
" names being imported incorrectly.
function! ale_linters#python#pyright#GetCwd(buffer) abort
    let l:fake_linter = {
    \   'name': 'pyright',
    \   'project_root': function('ale#python#FindProjectRoot'),
    \}
    let l:root = ale#lsp_linter#FindProjectRoot(a:buffer, l:fake_linter)

    return !empty(l:root) ? l:root : v:null
endfunction

function! ale_linters#python#pyright#GetConfig(buffer) abort
    let l:config = deepcopy(ale#Var(a:buffer, 'python_pyright_config'))

    if !has_key(l:config, 'python')
        let l:config.python = {}
    endif

    if type(l:config.python) is v:t_dict
        " Automatically detect the virtualenv path and use it.
        if !has_key(l:config.python, 'venvPath')
            let l:venv = ale#python#FindVirtualenv(a:buffer)

            if !empty(l:venv)
                let l:config.python.venvPath = l:venv
            endif
        endif

        " Automatically use the version of Python in virtualenv.
        if type(get(l:config.python, 'venvPath')) is v:t_string
        \&& !empty(l:config.python.venvPath)
        \&& !has_key(l:config.python, 'pythonPath')
            let l:config.python.pythonPath = ale#path#Simplify(
            \   l:config.python.venvPath
            \   . (has('win32') ? '/Scripts/python' : '/bin/python')
            \)
        endif
    endif

    return l:config
endfunction

function! ale_linters#python#pyright#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pyright_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pyright_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_pyright_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pyright', ['pyright-langserver'])
endfunction

function! ale_linters#python#pyright#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyright#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run pyright-langserver'
    \   : ''
    let l:env_string = ''

    if ale#Var(a:buffer, 'python_auto_virtualenv')
        let l:env_string = ale#python#AutoVirtualenvEnvString(a:buffer)
    endif

    return l:env_string . ale#Escape(l:executable) . l:exec_args . ' --stdio'
endfunction

call ale#linter#Define('python', {
\   'name': 'pyright',
\   'lsp': 'stdio',
\   'cwd': function('ale_linters#python#pyright#GetCwd'),
\   'executable': function('ale_linters#python#pyright#GetExecutable'),
\   'command': function('ale_linters#python#pyright#GetCommand'),
\   'project_root': function('ale#python#FindProjectRoot'),
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\   'lsp_config': function('ale_linters#python#pyright#GetConfig'),
\})
