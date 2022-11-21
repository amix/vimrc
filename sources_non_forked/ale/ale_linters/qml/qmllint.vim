" Author: pylipp (www.github.com/pylipp)
" Description: qmllint for QML files

" Find lines like
" /home/foo_user42/code-base/qml/Screen.qml:11 : Expected token `}'
function! ale_linters#qml#qmllint#Handle(buffer, lines) abort
    let l:pattern = '\v^[/_-a-zA-z0-9\. ]+:(\d+) : (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': 0,
        \   'text': l:match[2],
        \   'type': 'E',
        \}
        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('qml', {
\   'name': 'qmllint',
\   'output_stream': 'stderr',
\   'executable': 'qmllint',
\   'command': 'qmllint %t',
\   'callback': 'ale_linters#qml#qmllint#Handle',
\})
