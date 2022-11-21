" Author: Kevin Locke <kevin@kevinlocke.name>
" Description: pylama for python files

call ale#Set('python_pylama_executable', 'pylama')
call ale#Set('python_pylama_options', '')
call ale#Set('python_pylama_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pylama_auto_pipenv', 0)
call ale#Set('python_pylama_auto_poetry', 0)
call ale#Set('python_pylama_change_directory', 1)

function! ale_linters#python#pylama#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pylama_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pylama_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pylama', ['pylama'])
endfunction

function! ale_linters#python#pylama#RunWithVersionCheck(buffer) abort
    let l:executable = ale_linters#python#pylama#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run pylama'
    \   : ''

    let l:command = ale#Escape(l:executable) . l:exec_args . ' --version'

    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   l:executable,
    \   l:command,
    \   function('ale_linters#python#pylama#GetCommand'),
    \)
endfunction

function! ale_linters#python#pylama#GetCwd(buffer) abort
    if ale#Var(a:buffer, 'python_pylama_change_directory')
        " Pylama loads its configuration from the current directory only, and
        " applies file masks using paths relative to the current directory.
        " Run from project root, if found, otherwise buffer dir.
        let l:project_root = ale#python#FindProjectRoot(a:buffer)

        return !empty(l:project_root) ? l:project_root : '%s:h'
    endif

    return ''
endfunction

function! ale_linters#python#pylama#GetCommand(buffer, version) abort
    let l:executable = ale_linters#python#pylama#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run pylama'
    \   : ''

    " json format is added in version 8.1.4
    " https://github.com/klen/pylama/blob/develop/Changelog
    let l:format_json_args = ale#semver#GTE(a:version, [8, 1, 4])
    \   ? ' --format json'
    \   : ''

    " Note: Using %t to lint changes would be preferable, but many pylama
    " checks use surrounding paths (e.g. C0103 module name, E0402 relative
    " import beyond top, etc.).  Neither is ideal.
    return ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad(ale#Var(a:buffer, 'python_pylama_options'))
    \   . l:format_json_args
    \   . ' %s'
endfunction

function! ale_linters#python#pylama#Handle(buffer, version, lines) abort
    if empty(a:lines)
        return []
    endif

    let l:output = ale#python#HandleTraceback(a:lines, 1)

    " First letter of error code is a pylint-compatible message type
    " http://pylint.pycqa.org/en/latest/user_guide/output.html#source-code-analysis-section
    " D is for Documentation (pydocstyle)
    let l:pylint_type_to_ale_type = {
    \   'I': 'I',
    \   'R': 'W',
    \   'C': 'W',
    \   'W': 'W',
    \   'E': 'E',
    \   'F': 'E',
    \   'D': 'W',
    \}
    let l:pylint_type_to_ale_sub_type = {
    \   'R': 'style',
    \   'C': 'style',
    \   'D': 'style',
    \}

    if ale#semver#GTE(a:version, [8, 1, 4])
        try
            let l:errors = json_decode(join(a:lines, ''))
        catch
            return l:output
        endtry

        if empty(l:errors)
            return l:output
        endif

        for l:error in l:errors
            call add(l:output, {
            \   'lnum': l:error['lnum'],
            \   'col': l:error['col'],
            \   'code': l:error['number'],
            \   'type': get(l:pylint_type_to_ale_type, l:error['etype'], 'W'),
            \   'sub_type': get(l:pylint_type_to_ale_sub_type, l:error['etype'], ''),
            \   'text': printf('%s [%s]', l:error['message'], l:error['source']),
            \})
        endfor
    else
        let l:pattern = '\v^.{-}:([0-9]+):([0-9]+): +%(([A-Z][0-9]+):? +)?(.*)$'

        for l:match in ale#util#GetMatches(a:lines, l:pattern)
            call add(l:output, {
            \   'lnum': str2nr(l:match[1]),
            \   'col': str2nr(l:match[2]),
            \   'code': l:match[3],
            \   'type': get(l:pylint_type_to_ale_type, l:match[3][0], 'W'),
            \   'sub_type': get(l:pylint_type_to_ale_sub_type, l:match[3][0], ''),
            \   'text': l:match[4],
            \})
        endfor
    endif

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'pylama',
\   'executable': function('ale_linters#python#pylama#GetExecutable'),
\   'cwd': function('ale_linters#python#pylama#GetCwd'),
\   'command': function('ale_linters#python#pylama#RunWithVersionCheck'),
\   'callback': {buffer, lines -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#python#pylama#GetExecutable(buffer),
\       '%e --version',
\       {buffer, version -> ale_linters#python#pylama#Handle(
\           buffer,
\           l:version,
\           lines)},
\   )},
\   'lint_file': 1,
\})
