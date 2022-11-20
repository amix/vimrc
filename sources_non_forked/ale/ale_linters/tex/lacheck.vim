" Author: Andrew Balmos - <andrew@balmos.org>
" Description: lacheck for LaTeX files

call ale#Set('tex_lacheck_executable', 'lacheck')

function! ale_linters#tex#lacheck#Handle(buffer, lines) abort
    " Mattes lines like:
    "
    " "book.tex", line 37: possible unwanted space at "{"
    " "book.tex", line 38: missing `\ ' after "etc."
    let l:pattern = '^"\(.\+\)", line \(\d\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " lacheck follows `\input{}` commands. If the cwd is not the same as the
        " file in the buffer then it will fail to find the inputted items. We do not
        " want warnings from those items anyway
        if !empty(matchstr(l:match[3], '^Could not open ".\+"$'))
            continue
        endif

        " lacheck follows `\input{}` commands. We are only interested in
        " reporting errors for the current buffer only.
        if empty(matchstr(fnamemodify(l:match[1], ':t'), fnamemodify(bufname(a:buffer), ':t')))
            continue
        endif

        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('tex', {
\   'name': 'lacheck',
\   'executable': {b -> ale#Var(b, 'tex_lacheck_executable')},
\   'command': '%e %t',
\   'callback': 'ale_linters#tex#lacheck#Handle'
\})
