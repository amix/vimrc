" Author: Nils Leuzinger - https://github.com/PawkyPenguin
" Description: Basic scala support using fsc
"
function! ale_linters#scala#fsc#GetExecutable(buffer) abort
    if index(split(getbufvar(a:buffer, '&filetype'), '\.'), 'sbt') >= 0
        " Don't check sbt files
        return ''
    endif

    return 'fsc'
endfunction

function! ale_linters#scala#fsc#GetCommand(buffer) abort
    let l:executable = ale_linters#scala#fsc#GetExecutable(a:buffer)

    if empty(l:executable)
        return ''
    endif

    return ale#Escape(l:executable) . ' -Ystop-after:parser %t'
endfunction

call ale#linter#Define('scala', {
\   'name': 'fsc',
\   'executable_callback': 'ale_linters#scala#fsc#GetExecutable',
\   'command_callback': 'ale_linters#scala#fsc#GetCommand',
\   'callback': 'ale#handlers#scala#HandleScalacLintFormat',
\   'output_stream': 'stderr',
\})
