scriptencoding utf-8
" Author: Johannes Wienke <languitar@semipol.de>
" Description: Error handling for errors in alex output format

function! ale#handlers#alex#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'alex', [
    \   'node_modules/.bin/alex',
    \   'node_modules/alex/cli.js',
    \])
endfunction

function! ale#handlers#alex#CreateCommandCallback(flags) abort
    return {b -> ale#node#Executable(b, ale#handlers#alex#GetExecutable(b))
    \            . ' %s '
    \            . a:flags}
endfunction

function! ale#handlers#alex#Handle(buffer, lines) abort
    " Example output:
    "       6:256-6:262  warning  Be careful with “killed”, it’s profane in some cases      killed           retext-profanities
    let l:pattern = '\v^ *(\d+):(\d+)-(\d+):(\d+) +warning +(.{-})  +(.{-})  +(.{-})$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'end_lnum': l:match[3] + 0,
        \   'end_col': l:match[4] - 1,
        \   'text': l:match[5] . ' (' . (l:match[7]) . ')',
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

" Define a linter for a specific filetype. Accept flags to adapt to the filetype.
"    no flags  treat input as markdown
"    --html    treat input as HTML
"    --text    treat input as plaintext
function! ale#handlers#alex#DefineLinter(filetype, flags) abort
    call ale#Set('alex_executable', 'alex')
    call ale#Set('alex_use_global', get(g:, 'ale_use_global_executables', 0))

    call ale#linter#Define(a:filetype, {
    \   'name': 'alex',
    \   'executable': function('ale#handlers#alex#GetExecutable'),
    \   'command': ale#handlers#alex#CreateCommandCallback(a:flags),
    \   'output_stream': 'stderr',
    \   'callback': 'ale#handlers#alex#Handle',
    \   'lint_file': 1,
    \})
endfunction
