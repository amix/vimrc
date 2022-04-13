" Author: Karl Bartel <karl42@gmail.com> - http://karl.berlin/
" Description: Report solc compiler errors in Solidity code

call ale#Set('solidity_solc_executable', 'solc')
call ale#Set('solidity_solc_options', '')

function! ale_linters#solidity#solc#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " Error: Expected ';' but got '('
    "    --> /path/to/file/file.sol:1:10:)
    let l:pattern = '\v(Error|Warning): (.*)$'
    let l:line_and_column_pattern = '\v\.sol:(\d+):(\d+):'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            let l:match = matchlist(l:line, l:line_and_column_pattern)

            if len(l:match) > 0
                let l:index = len(l:output) - 1
                let l:output[l:index]['lnum'] = l:match[1] + 0
                let l:output[l:index]['col'] = l:match[2] + 0
            endif
        else
            let l:isError = l:match[1] is? 'Error'

            call add(l:output, {
            \   'lnum': 0,
            \   'col': 0,
            \   'text': l:match[2],
            \   'type': l:isError ? 'E' : 'W',
            \})
        endif
    endfor

    return l:output
endfunction

function! ale_linters#solidity#solc#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'solidity_solc_executable')

    return l:executable . ale#Pad(ale#Var(a:buffer, 'solidity_solc_options')) . ' %s'
endfunction

call ale#linter#Define('solidity', {
\   'name': 'solc',
\   'executable': {b -> ale#Var(b, 'solidity_solc_executable')},
\   'command': function('ale_linters#solidity#solc#GetCommand'),
\   'callback': 'ale_linters#solidity#solc#Handle',
\   'output_stream': 'stderr',
\})
