" Author: Zoltan Kalmar - https://github.com/kalmiz,
"         w0rp <devw0rp@gmail.com>
" Description: Basic scala support using scalac

function! ale_linters#scala#scalac#GetExecutable(buffer) abort
    if index(split(getbufvar(a:buffer, '&filetype'), '\.'), 'sbt') >= 0
        " Don't check sbt files with scalac.
        return ''
    endif

    return 'scalac'
endfunction

function! ale_linters#scala#scalac#GetCommand(buffer) abort
    let l:executable = ale_linters#scala#scalac#GetExecutable(a:buffer)

    if empty(l:executable)
        return ''
    endif

    return ale#Escape(l:executable) . ' -Ystop-after:parser %t'
endfunction

function! ale_linters#scala#scalac#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " /var/folders/5q/20rgxx3x1s34g3m14n5bq0x80000gn/T/vv6pSsy/0:26: error: expected class or object definition
    let l:pattern = '^.\+:\(\d\+\): \(\w\+\): \(.\+\)'
    let l:output = []
    let l:ln = 0

    for l:line in a:lines
        let l:ln = l:ln + 1
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) == 0
            continue
        endif

        let l:text = l:match[3]
        let l:type = l:match[2] is# 'error' ? 'E' : 'W'
        let l:col = 0

        if l:ln + 1 < len(a:lines)
            let l:col = stridx(a:lines[l:ln + 1], '^')
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:col + 1,
        \   'text': l:text,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('scala', {
\   'name': 'scalac',
\   'executable_callback': 'ale_linters#scala#scalac#GetExecutable',
\   'command_callback': 'ale_linters#scala#scalac#GetCommand',
\   'callback': 'ale_linters#scala#scalac#Handle',
\   'output_stream': 'stderr',
\})
