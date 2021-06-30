call ale#Set('python_pyright_executable', 'pyright-langserver')
call ale#Set('python_pyright_config', {})

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

call ale#linter#Define('python', {
\   'name': 'pyright',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'python_pyright_executable')},
\   'command': '%e --stdio',
\   'project_root': function('ale#python#FindProjectRoot'),
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\   'lsp_config': function('ale_linters#python#pyright#GetConfig'),
\})
