" Author: Zoltan Kalmar - https://github.com/kalmiz,
"         w0rp <devw0rp@gmail.com>
" Description: Basic scala support using scalac

function! s:IsSbt(buffer) abort
    return index(split(getbufvar(a:buffer, '&filetype'), '\.'), 'sbt') >= 0
endfunction

call ale#linter#Define('scala', {
\   'name': 'scalac',
\   'executable': {buf -> s:IsSbt(buf) ? '' : 'scalac'},
\   'command': '%e -Ystop-after:parser %t',
\   'callback': 'ale#handlers#scala#HandleScalacLintFormat',
\   'output_stream': 'stderr',
\})
