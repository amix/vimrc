" Author: zanona <https://github.com/zanona>, w0rp <devw0rp@gmail.com>
" Description: This file adds support for checking Less code with lessc.

call ale#Set('less_lessc_executable', 'lessc')
call ale#Set('less_lessc_options', '')
call ale#Set('less_lessc_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#less#lessc#GetCommand(buffer) abort
    return '%e --no-color --lint'
    \   . ' --include-path=' . ale#Escape(expand('#' . a:buffer . ':p:h'))
    \   . ale#Pad(ale#Var(a:buffer, 'less_lessc_options'))
    \   . ' -'
endfunction

function! ale_linters#less#lessc#Handle(buffer, lines) abort
    let l:dir = expand('#' . a:buffer . ':p:h')
    " Matches patterns like the following:
    let l:pattern = '^\(\w\+\): \(.\{-}\) in \(.\{-}\) on line \(\d\+\), column \(\d\+\):$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[4] + 0,
        \   'col': l:match[5] + 0,
        \   'text': l:match[2],
        \   'type': 'E',
        \}

        if l:match[3] isnot# '-'
            let l:item.filename = ale#path#GetAbsPath(l:dir, l:match[3])
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('less', {
\   'name': 'lessc',
\   'executable': {b -> ale#node#FindExecutable(b, 'less_lessc', [
\       'node_modules/.bin/lessc',
\   ])},
\   'command': function('ale_linters#less#lessc#GetCommand'),
\   'callback': 'ale_linters#less#lessc#Handle',
\   'output_stream': 'stderr',
\})
