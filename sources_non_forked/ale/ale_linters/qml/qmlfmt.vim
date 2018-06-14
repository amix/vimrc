" Author: pylipp (www.github.com/pylipp)
" Description: qmlfmt for QML files

let g:ale_qml_qmlfmt_executable = get(g:, 'ale_qml_qmlfmt_executable', 'qmlfmt')

function! ale_linters#qml#qmlfmt#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'qml_qmlfmt_executable')
endfunction

function! ale_linters#qml#qmlfmt#GetCommand(buffer) abort
    return ale#Escape(ale_linters#qml#qmlfmt#GetExecutable(a:buffer))
    \   . ' -e'
endfunction

" Find lines like
" Error:11:1: Expected token `}'
function! ale_linters#qml#qmlfmt#Handle(buffer, lines) abort
    let l:pattern = '\v^(Error|Warning):(\d+):(\d+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': l:match[1] is# 'Warning' ? 'W' : 'E',
        \}
        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('qml', {
\   'name': 'qmlfmt',
\   'output_stream': 'stderr',
\   'executable_callback': 'ale_linters#qml#qmlfmt#GetExecutable',
\   'command_callback': 'ale_linters#qml#qmlfmt#GetCommand',
\   'callback': 'ale_linters#qml#qmlfmt#Handle',
\})
