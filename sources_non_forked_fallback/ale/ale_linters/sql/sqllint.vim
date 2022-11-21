" ale_linters/sql/sqllint.vim
" Author: Joe Reynolds <joereynolds952@gmail.co>
" Description: sql-lint for SQL files.
"              sql-lint can be found at
"              https://www.npmjs.com/package/sql-lint
"              https://github.com/joereynolds/sql-lint

function! ale_linters#sql#sqllint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " stdin:1 [ER_NO_DB_ERROR] No database selected
    let l:pattern = '\v^[^:]+:(\d+) (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3][0],
        \   'text': l:match[0],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('sql', {
\   'name': 'sqllint',
\   'aliases': ['sql-lint'],
\   'executable': 'sql-lint',
\   'command': 'sql-lint',
\   'callback': 'ale_linters#sql#sqllint#Handle',
\})
