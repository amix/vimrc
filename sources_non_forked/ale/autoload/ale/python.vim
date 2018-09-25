" Author: w0rp <devw0rp@gmail.com>
" Description: Functions for integrating with Python linters.

call ale#Set('python_auto_pipenv', '0')

let s:sep = has('win32') ? '\' : '/'
" bin is used for Unix virtualenv directories, and Scripts is for Windows.
let s:bin_dir = has('unix') ? 'bin' : 'Scripts'
let g:ale_virtualenv_dir_names = get(g:, 'ale_virtualenv_dir_names', [
\   '.env',
\   '.venv',
\   'env',
\   've-py3',
\   've',
\   'virtualenv',
\   'venv',
\])

function! ale#python#FindProjectRootIni(buffer) abort
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        " If you change this, update ale-python-root documentation.
        if filereadable(l:path . '/MANIFEST.in')
        \|| filereadable(l:path . '/setup.cfg')
        \|| filereadable(l:path . '/pytest.ini')
        \|| filereadable(l:path . '/tox.ini')
        \|| filereadable(l:path . '/mypy.ini')
        \|| filereadable(l:path . '/pycodestyle.cfg')
        \|| filereadable(l:path . '/flake8.cfg')
        \|| filereadable(l:path . '/.flake8rc')
        \|| filereadable(l:path . '/Pipfile')
        \|| filereadable(l:path . '/Pipfile.lock')
            return l:path
        endif
    endfor

    return ''
endfunction

" Given a buffer number, find the project root directory for Python.
" The root directory is defined as the first directory found while searching
" upwards through paths, including the current directory, until a path
" containing an init file (one from MANIFEST.in, setup.cfg, pytest.ini,
" tox.ini) is found. If it is not possible to find the project root directory
" via init file, then it will be defined as the first directory found
" searching upwards through paths, including the current directory, until no
" __init__.py files is found.
function! ale#python#FindProjectRoot(buffer) abort
    let l:ini_root = ale#python#FindProjectRootIni(a:buffer)

    if !empty(l:ini_root)
      return l:ini_root
    endif

    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        if !filereadable(l:path . '/__init__.py')
            return l:path
        endif
    endfor

    return ''
endfunction

" Given a buffer number, find a virtualenv path for Python.
function! ale#python#FindVirtualenv(buffer) abort
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        " Skip empty path components returned in MSYS.
        if empty(l:path)
            continue
        endif

        for l:dirname in ale#Var(a:buffer, 'virtualenv_dir_names')
            let l:venv_dir = ale#path#Simplify(
            \   join([l:path, l:dirname], s:sep)
            \)
            let l:script_filename = ale#path#Simplify(
            \   join([l:venv_dir, s:bin_dir, 'activate'], s:sep)
            \)

            if filereadable(l:script_filename)
                return l:venv_dir
            endif
        endfor
    endfor

    return $VIRTUAL_ENV
endfunction

" Given a buffer number and a command name, find the path to the executable.
" First search on a virtualenv for Python, if nothing is found, try the global
" command. Returns an empty string if cannot find the executable
function! ale#python#FindExecutable(buffer, base_var_name, path_list) abort
    if ale#Var(a:buffer, a:base_var_name . '_use_global')
        return ale#Var(a:buffer, a:base_var_name . '_executable')
    endif

    let l:virtualenv = ale#python#FindVirtualenv(a:buffer)

    if !empty(l:virtualenv)
        for l:path in a:path_list
            let l:ve_executable = ale#path#Simplify(
            \   join([l:virtualenv, s:bin_dir, l:path], s:sep)
            \)

            if executable(l:ve_executable)
                return l:ve_executable
            endif
        endfor
    endif

    return ale#Var(a:buffer, a:base_var_name . '_executable')
endfunction

" Detects whether a pipenv environment is present.
function! ale#python#PipenvPresent(buffer) abort
    return findfile('Pipfile.lock', expand('#' . a:buffer . ':p:h') . ';') isnot# ''
endfunction
