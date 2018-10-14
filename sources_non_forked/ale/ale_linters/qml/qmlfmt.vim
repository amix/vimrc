" Author: pylipp (www.github.com/pylipp)
" Description: qmlfmt for QML files

call ale#Set('qml_qmlfmt_executable', 'qmlfmt')

" Find lines like
" Error:11:1: Expected token `}'
function! ale_linters#qml#qmlfmt#Handle(buffer, lines) abort
    let l:pattern = '\v^(Error|Warning):(\d+):(\d+): (.+)$'

    return map(ale#util#GetMatches(a:lines, l:pattern), "{
    \   'lnum': v:val[2] + 0,
    \   'col': v:val[3] + 0,
    \   'text': v:val[4],
    \   'type': v:val[1] is# 'Warning' ? 'W' : 'E',
    \}")
endfunction

call ale#linter#Define('qml', {
\   'name': 'qmlfmt',
\   'output_stream': 'stderr',
\   'executable_callback': ale#VarFunc('qml_qmlfmt_executable'),
\   'command': '%e -e',
\   'callback': 'ale_linters#qml#qmlfmt#Handle',
\})
