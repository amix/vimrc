" Author: w0rp <dev@w0rp.com>
" Description: Functions for integrating with Python linters.

call ale#Set('python_auto_pipenv', '0')
call ale#Set('python_auto_poetry', '0')

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
        \|| filereadable(l:path . '/.pyre_configuration.local')
        \|| filereadable(l:path . '/mypy.ini')
        \|| filereadable(l:path . '/.mypy.ini')
        \|| filereadable(l:path . '/pycodestyle.cfg')
        \|| filereadable(l:path . '/.flake8')
        \|| filereadable(l:path . '/.flake8rc')
        \|| filereadable(l:path . '/pylama.ini')
        \|| filereadable(l:path . '/pylintrc')
        \|| filereadable(l:path . '/.pylintrc')
        \|| filereadable(l:path . '/pyrightconfig.json')
        \|| filereadable(l:path . '/pyrightconfig.toml')
        \|| filereadable(l:path . '/Pipfile')
        \|| filereadable(l:path . '/Pipfile.lock')
        \|| filereadable(l:path . '/poetry.lock')
        \|| filereadable(l:path . '/pyproject.toml')
        \|| filereadable(l:path . '/.tool-versions')
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

" Automatically determine virtualenv environment variables and build
" a string of them to prefix linter commands with.
function! ale#python#AutoVirtualenvEnvString(buffer) abort
    let l:venv_dir = ale#python#FindVirtualenv(a:buffer)

    if !empty(l:venv_dir)
        let l:strs = [ ]

        " expand PATH correctly inside of the appropriate shell.
        if has('win32')
            call add(l:strs, 'set PATH=' . ale#Escape(l:venv_dir) . ';%PATH% && ')
        else
            call add(l:strs, 'PATH=' . ale#Escape(l:venv_dir) . '":$PATH" ')
        endif

        return join(l:strs, '')
    endif

    return ''
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

" Handle traceback.print_exception() output starting in the first a:limit lines.
function! ale#python#HandleTraceback(lines, limit) abort
    let l:nlines = len(a:lines)
    let l:limit = a:limit > l:nlines ? l:nlines : a:limit
    let l:start = 0

    while l:start < l:limit
        if a:lines[l:start] is# 'Traceback (most recent call last):'
            break
        endif

        let l:start += 1
    endwhile

    if l:start >= l:limit
        return []
    endif

    let l:end = l:start + 1

    " Traceback entries are always prefixed with 2 spaces.
    " SyntaxError marker (if present) is prefixed with at least 4 spaces.
    " Final exc line starts with exception class name (never a space).
    while l:end < l:nlines && a:lines[l:end][0] is# ' '
        let l:end += 1
    endwhile

    let l:exc_line = l:end < l:nlines
    \   ? a:lines[l:end]
    \   : 'An exception was thrown.'

    return [{
    \   'lnum': 1,
    \   'text': l:exc_line . ' (See :ALEDetail)',
    \   'detail': join(a:lines[(l:start):(l:end)], "\n"),
    \}]
endfunction

" Detects whether a pipenv environment is present.
function! ale#python#PipenvPresent(buffer) abort
    return findfile('Pipfile.lock', expand('#' . a:buffer . ':p:h') . ';') isnot# ''
endfunction

" Detects whether a poetry environment is present.
function! ale#python#PoetryPresent(buffer) abort
    return findfile('poetry.lock', expand('#' . a:buffer . ':p:h') . ';') isnot# ''
endfunction
