" Author: w0rp <devw0rp@gmail.com>
" Description: flake8 for python files

call ale#Set('python_flake8_executable', 'flake8')
call ale#Set('python_flake8_options', '')
call ale#Set('python_flake8_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_flake8_change_directory', 1)
call ale#Set('python_flake8_auto_pipenv', 0)

function! s:UsingModule(buffer) abort
    return ale#Var(a:buffer, 'python_flake8_options') =~# ' *-m flake8'
endfunction

function! ale_linters#python#flake8#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_flake8_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if !s:UsingModule(a:buffer)
        return ale#python#FindExecutable(a:buffer, 'python_flake8', ['flake8'])
    endif

    return ale#Var(a:buffer, 'python_flake8_executable')
endfunction

function! ale_linters#python#flake8#VersionCheck(buffer) abort
    let l:executable = ale_linters#python#flake8#GetExecutable(a:buffer)

    " If we have previously stored the version number in a cache, then
    " don't look it up again.
    if ale#semver#HasVersion(l:executable)
        " Returning an empty string skips this command.
        return ''
    endif

    let l:executable = ale#Escape(l:executable)
    let l:module_string = s:UsingModule(a:buffer) ? ' -m flake8' : ''

    return l:executable . l:module_string . ' --version'
endfunction

function! ale_linters#python#flake8#GetCommand(buffer, version_output) abort
    let l:cd_string = ale#Var(a:buffer, 'python_flake8_change_directory')
    \   ? ale#path#BufferCdString(a:buffer)
    \   : ''
    let l:executable = ale_linters#python#flake8#GetExecutable(a:buffer)
    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run flake8'
    \   : ''

    " Only include the --stdin-display-name argument if we can parse the
    " flake8 version, and it is recent enough to support it.
    let l:display_name_args = ale#semver#GTE(l:version, [3, 0, 0])
    \   ? ' --stdin-display-name %s'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_flake8_options')

    return l:cd_string
    \   . ale#Escape(l:executable) . l:exec_args
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --format=default'
    \   . l:display_name_args . ' -'
endfunction

let s:end_col_pattern_map = {
\   'F405': '\(.\+\) may be undefined',
\   'F821': 'undefined name ''\([^'']\+\)''',
\   'F999': '^''\([^'']\+\)''',
\   'F841': 'local variable ''\([^'']\+\)''',
\}

function! ale_linters#python#flake8#Handle(buffer, lines) abort
    let l:output = ale#python#HandleTraceback(a:lines, 10)

    if !empty(l:output)
        return l:output
    endif

    " Matches patterns line the following:
    "
    " Matches patterns line the following:
    "
    " stdin:6:6: E111 indentation is not a multiple of four
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):?(\d+)?: ([[:alnum:]]+):? (.*)$'

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[3]

        if (l:code is# 'W291' || l:code is# 'W293')
        \ && !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
            " Skip warnings for trailing whitespace if the option is off.
            continue
        endif

        if l:code is# 'W391'
        \&& !ale#Var(a:buffer, 'warn_about_trailing_blank_lines')
            " Skip warnings for trailing blank lines if the option is off
            continue
        endif

        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'vcol': 1,
        \   'text': l:match[4],
        \   'code': l:code,
        \   'type': 'W',
        \}

        if l:code[:0] is# 'F'
            if l:code isnot# 'F401'
                let l:item.type = 'E'
            endif
        elseif l:code[:0] is# 'E'
            let l:item.type = 'E'

            if l:code isnot# 'E999' && l:code isnot# 'E112'
                let l:item.sub_type = 'style'
            endif
        elseif l:code[:0] is# 'W'
            let l:item.sub_type = 'style'
        endif

        let l:end_col_pattern = get(s:end_col_pattern_map, l:code, '')

        if !empty(l:end_col_pattern)
            let l:end_col_match = matchlist(l:match[4], l:end_col_pattern)

            if !empty(l:end_col_match)
                let l:item.end_col = l:item.col + len(l:end_col_match[1]) - 1
            endif
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'flake8',
\   'executable': function('ale_linters#python#flake8#GetExecutable'),
\   'command_chain': [
\       {'callback': 'ale_linters#python#flake8#VersionCheck'},
\       {'callback': 'ale_linters#python#flake8#GetCommand', 'output_stream': 'both'},
\   ],
\   'callback': 'ale_linters#python#flake8#Handle',
\})
