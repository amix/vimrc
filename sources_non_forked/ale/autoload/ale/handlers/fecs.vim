" Author: harttle <yangjvn@126.com>
" Description: fecs http://fecs.baidu.com/

call ale#Set('javascript_fecs_executable', 'fecs')
call ale#Set('javascript_fecs_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#handlers#fecs#GetCommand(buffer) abort
    return '%e check --colors=false --rule=true %t'
endfunction

function! ale#handlers#fecs#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_fecs', [
    \   'node_modules/.bin/fecs',
    \   'node_modules/fecs/bin/fecs',
    \])
endfunction

function! ale#handlers#fecs#Handle(buffer, lines) abort
    " Matches patterns looking like the following
    "
    " fecs  WARN → line 20, col 25: Unexpected console statement.     (no-console)
    " fecs ERROR → line 24, col 36: Missing radix parameter.  (radix)
    "
    let l:pattern = '\v^.*(WARN|ERROR)\s+→\s+line (\d+),\s+col\s+(\d+):\s+(.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:obj = {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4]
        \}

        let l:code_match = matchlist(l:match[4], '\v^(.{-})\s*\((.+)\)$')

        if !empty(l:code_match)
            let l:obj.code = l:code_match[2]
            let l:obj.text = l:code_match[1]
        endif

        if l:match[1] is# 'WARN'
            let l:obj.type = 'W'
        elseif l:match[1] is# 'ERROR'
            let l:obj.type = 'E'
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

