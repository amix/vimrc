" Author: Nils Leuzinger - https://github.com/PawkyPenguin
" Description: Basic scala support using fsc

function! s:IsSbt(buffer) abort
    return index(split(getbufvar(a:buffer, '&filetype'), '\.'), 'sbt') >= 0
endfunction

call ale#linter#Define('scala', {
\   'name': 'fsc',
\   'executable': {buf -> s:IsSbt(buf) ? '' : 'fsc'},
\   'command': '%e -Ystop-after:parser %t',
\   'callback': 'ale#handlers#scala#HandleScalacLintFormat',
\   'output_stream': 'stderr',
\})
