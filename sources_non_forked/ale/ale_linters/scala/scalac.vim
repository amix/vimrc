" Author: Zoltan Kalmar - https://github.com/kalmiz,
"         w0rp <devw0rp@gmail.com>
" Description: Basic scala support using scalac

function! ale_linters#scala#scalac#GetExecutable(buffer) abort
    if index(split(getbufvar(a:buffer, '&filetype'), '\.'), 'sbt') >= 0
        " Don't check sbt files
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

call ale#linter#Define('scala', {
\   'name': 'scalac',
\   'executable_callback': 'ale_linters#scala#scalac#GetExecutable',
\   'command_callback': 'ale_linters#scala#scalac#GetCommand',
\   'callback': 'ale#handlers#scala#HandleScalacLintFormat',
\   'output_stream': 'stderr',
\})
