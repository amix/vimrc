" Author: Karl Bartel <karl42@gmail.com> - http://karl.berlin/
" Description: Report solc compiler errors in Solidity code

call ale#Set('solidity_solc_options', '')

function! ale_linters#solidity#solc#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " /path/to/file/file.sol:1:10: Error: Identifier not found or not unique.
    let l:pattern = '\v^[^:]+:(\d+):(\d+): (Error|Warning): (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:isError = l:match[3] is? 'error'
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'type': l:isError ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

function! ale_linters#solidity#solc#GetCommand(buffer) abort
    return 'solc' . ale#Pad(ale#Var(a:buffer, 'solidity_solc_options')) . ' %s'
endfunction

call ale#linter#Define('solidity', {
\   'name': 'solc',
\   'executable': 'solc',
\   'command': function('ale_linters#solidity#solc#GetCommand'),
\   'callback': 'ale_linters#solidity#solc#Handle',
\   'output_stream': 'stderr',
\})
