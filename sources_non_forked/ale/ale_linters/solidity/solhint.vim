" Author: Franco Victorio - https://github.com/fvictorio
" Description: Report errors in Solidity code with solhint

function! ale_linters#solidity#solhint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " /path/to/file/file.sol: line 1, col 10, Error - 'addOne' is defined but never used. (no-unused-vars)

    let l:pattern = '\v^[^:]+: line (\d+), col (\d+), (Error|Warning) - (.*) \((.*)\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:isError = l:match[3] is? 'error'
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'code': l:match[5],
        \   'type': l:isError ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('solidity', {
\   'name': 'solhint',
\   'executable': 'solhint',
\   'command': 'solhint --formatter compact %t',
\   'callback': 'ale_linters#solidity#solhint#Handle',
\})
