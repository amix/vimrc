" Author: chocoelho <carlospecter@gmail.com>
" Description: prospector linter python files

call ale#Set('python_prospector_auto_pipenv', 0)

let g:ale_python_prospector_executable =
\   get(g:, 'ale_python_prospector_executable', 'prospector')

let g:ale_python_prospector_options =
\   get(g:, 'ale_python_prospector_options', '')

let g:ale_python_prospector_use_global = get(g:, 'ale_python_prospector_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#python#prospector#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_prospector_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_prospector', ['prospector'])
endfunction

function! ale_linters#python#prospector#GetCommand(buffer) abort
    let l:executable = ale_linters#python#prospector#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run prospector'
    \   : ''

    return ale#Escape(l:executable)
    \   . l:exec_args
    \   . ' ' . ale#Var(a:buffer, 'python_prospector_options')
    \   . ' --messages-only --absolute-paths --zero-exit --output-format json'
    \   . ' %s'
endfunction

function! ale_linters#python#prospector#Handle(buffer, lines) abort
    let l:output = []

    if empty(a:lines)
        return []
    endif

    let l:prospector_error = json_decode(join(a:lines, ''))

    for l:error in l:prospector_error.messages
        if (l:error.code is# 'W291' || l:error.code is# 'W293' || l:error.code is# 'trailing-whitespace')
        \ && !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
            " Skip warnings for trailing whitespace if the option is off.
            continue
        endif

        if l:error.code is# 'W391'
        \&& !ale#Var(a:buffer, 'warn_about_trailing_blank_lines')
            " Skip warnings for trailing blank lines if the option is off
            continue
        endif

        if l:error.source =~# '\v\[%(dodgy|mccabe|pep8|pep257|pyroma)\]$'
            let l:sub_type = 'style'
        else
            let l:sub_type = ''
        endif

        if l:error.source =~# '\v\[pylint\]$'
            let l:type = l:error.code =~? '\m^[CRW]' ? 'W' : 'E'
        elseif l:error.source =~# '\v\[%(frosted|pep8)\]$'
            let l:type = l:error.code =~? '\m^W' ? 'W' : 'E'
        elseif l:error.source =~# '\v\[%(dodgy|pyroma|vulture)\]$'
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        let l:item = {
        \   'lnum': l:error.location.line,
        \   'col': l:error.location.character + 1,
        \   'text': l:error.message,
        \   'code': printf('(%s) %s', l:error.source, l:error.code),
        \   'type': l:type,
        \   'sub_type': l:sub_type,
        \}

        if l:sub_type is# ''
            unlet l:item.sub_type
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'prospector',
\   'executable_callback': 'ale_linters#python#prospector#GetExecutable',
\   'command_callback': 'ale_linters#python#prospector#GetCommand',
\   'callback': 'ale_linters#python#prospector#Handle',
\   'lint_file': 1,
\})
