" Author: Spencer Wood <https://github.com/scwood>, Adriaan Zonnenberg <amz@adriaan.xyz>
" Description: This file adds support for checking PHP with php-cli

call ale#Set('php_php_executable', 'php')

function! ale_linters#php#php#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " PHP 7.1<= - Parse error:  syntax error, unexpected ';', expecting ']' in - on line 15
    " PHP 7.2>= - Parse error:  syntax error, unexpected ';', expecting ']' in Standard input code on line 15
    let l:pattern = '\v^%(Fatal|Parse) error:\s+(.+unexpected ''(.+)%(expecting.+)@<!''.*|.+) in %(-|Standard input code) on line (\d+)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:col = empty(l:match[2]) ? 0 : stridx(getline(l:match[3]), l:match[2]) + 1

        let l:obj = {
        \   'lnum': l:match[3] + 0,
        \   'col': l:col,
        \   'text': l:match[1],
        \}

        if l:col != 0
            let l:obj.end_col = l:col + strlen(l:match[2]) - 1
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'php',
\   'executable': {b -> ale#Var(b, 'php_php_executable')},
\   'output_stream': 'stdout',
\   'command': '%e -l -d error_reporting=E_ALL -d display_errors=1 -d log_errors=0 --',
\   'callback': 'ale_linters#php#php#Handle',
\})
