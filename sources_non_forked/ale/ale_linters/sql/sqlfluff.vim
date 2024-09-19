" Author: Carl Smedstad <carl.smedstad at protonmail dot com>
" Description: sqlfluff for SQL files

let g:ale_sql_sqlfluff_executable =
\   get(g:, 'ale_sql_sqlfluff_executable', 'sqlfluff')

let g:ale_sql_sqlfluff_options =
\   get(g:, 'ale_sql_sqlfluff_options', '')

function! ale_linters#sql#sqlfluff#Executable(buffer) abort
    return ale#Var(a:buffer, 'sql_sqlfluff_executable')
endfunction

function! ale_linters#sql#sqlfluff#Command(buffer, version) abort
    let l:executable = ale_linters#sql#sqlfluff#Executable(a:buffer)
    let l:options = ale#Var(a:buffer, 'sql_sqlfluff_options')

    let l:cmd =
    \    ale#Escape(l:executable)
    \    . ' lint'

    let l:config_file = ale#path#FindNearestFile(a:buffer, '.sqlfluff')

    if !empty(l:config_file)
        let l:cmd .= ' --config ' . ale#Escape(l:config_file)
    else
        let l:cmd .= ' --dialect ansi'
    endif

    let l:cmd .=
    \   ' --format json '
    \   . l:options
    \   . ' %t'

    return l:cmd
endfunction

function! ale_linters#sql#sqlfluff#Handle(buffer, version, lines) abort
    let l:output = []
    let l:json_lines = ale#util#FuzzyJSONDecode(a:lines, [])

    if empty(l:json_lines)
        return l:output
    endif

    let l:json = l:json_lines[0]

    " if there's no warning, 'result' is `null`.
    if empty(get(l:json, 'violations'))
        return l:output
    endif

    if ale#semver#GTE(a:version, [3, 0, 0])
        for l:violation in get(l:json, 'violations', [])
            call add(l:output, {
            \   'filename': l:json.filepath,
            \   'lnum': l:violation.start_line_no,
            \   'end_lnum': l:violation.end_line_no,
            \   'col': l:violation.start_line_pos,
            \   'end_col': l:violation.end_line_pos,
            \   'text': l:violation.description,
            \   'code': l:violation.code,
            \   'type': 'W',
            \})
        endfor
    else
        for l:violation in get(l:json, 'violations', [])
            call add(l:output, {
            \   'filename': l:json.filepath,
            \   'lnum': l:violation.line_no,
            \   'col': l:violation.line_pos,
            \   'text': l:violation.description,
            \   'code': l:violation.code,
            \   'type': 'W',
            \})
        endfor
    endif

    return l:output
endfunction

call ale#linter#Define('sql', {
\   'name': 'sqlfluff',
\   'executable': function('ale_linters#sql#sqlfluff#Executable'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#sql#sqlfluff#Executable(buffer),
\       '%e --version',
\       function('ale_linters#sql#sqlfluff#Command'),
\   )},
\   'callback': {buffer, lines -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#sql#sqlfluff#Executable(buffer),
\       '%e --version',
\       {buffer, version -> ale_linters#sql#sqlfluff#Handle(
\           buffer,
\           l:version,
\           lines)},
\   )},
\})
