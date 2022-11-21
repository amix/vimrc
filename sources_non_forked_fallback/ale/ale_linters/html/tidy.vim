" Author: KabbAmine <amine.kabb@gmail.com>
" Description: This file adds support for checking HTML code with tidy.

let g:ale_html_tidy_executable = get(g:, 'ale_html_tidy_executable', 'tidy')
let g:ale_html_tidy_options = get(g:, 'ale_html_tidy_options', '-q -e -language en')

function! ale_linters#html#tidy#GetCommand(buffer) abort
    " Specify file encoding in options
    " (Idea taken from https://github.com/scrooloose/syntastic/blob/master/syntax_checkers/html/tidy.vim)
    let l:file_encoding = get({
    \   'ascii':        '-ascii',
    \   'big5':         '-big5',
    \   'cp1252':       '-win1252',
    \   'cp850':        '-ibm858',
    \   'cp932':        '-shiftjis',
    \   'iso-2022-jp':  '-iso-2022',
    \   'latin1':       '-latin1',
    \   'macroman':     '-mac',
    \   'sjis':         '-shiftjis',
    \   'utf-16le':     '-utf16le',
    \   'utf-16':       '-utf16',
    \   'utf-8':        '-utf8',
    \ }, &fileencoding, '-utf8')

    " On macOS, old tidy (released on 31 Oct 2006) is installed. It does not
    " consider HTML5 so we should avoid it.
    let l:executable = ale#Var(a:buffer, 'html_tidy_executable')

    if has('mac') && l:executable is# 'tidy' && exists('*exepath')
    \  && exepath(l:executable) is# '/usr/bin/tidy'
        return ''
    endif

    return printf('%s %s %s -',
    \   l:executable,
    \   ale#Var(a:buffer, 'html_tidy_options'),
    \   l:file_encoding
    \)
endfunction

function! ale_linters#html#tidy#Handle(buffer, lines) abort
    " Matches patterns lines like the following:
    " line 7 column 5 - Warning: missing </title> before </head>
    let l:pattern = '^line \(\d\+\) column \(\d\+\) - \(Warning\|Error\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:line = l:match[1] + 0
        let l:col = l:match[2] + 0
        let l:type = l:match[3] is# 'Error' ? 'E' : 'W'
        let l:text = l:match[4]

        call add(l:output, {
        \   'lnum': l:line,
        \   'col': l:col,
        \   'text': l:text,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('html', {
\   'name': 'tidy',
\   'executable': {b -> ale#Var(b, 'html_tidy_executable')},
\   'output_stream': 'stderr',
\   'command': function('ale_linters#html#tidy#GetCommand'),
\   'callback': 'ale_linters#html#tidy#Handle',
\ })
