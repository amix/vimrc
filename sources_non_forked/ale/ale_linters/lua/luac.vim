" Author: Jon Xie https://github.com/xiejiangzhi
" Description: luac linter for lua files

call ale#Set('lua_luac_executable', 'luac')

function! ale_linters#lua#luac#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'lua_luac_executable')
endfunction

function! ale_linters#lua#luac#GetCommand(buffer) abort
    let l:executable = ale_linters#lua#luac#GetExecutable(a:buffer)
    return ale#Escape(l:executable) . ' -p - '
endfunction

function! ale_linters#lua#luac#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " luac: stdin:5: '=' expected near ')'
    " luac: stdin:8: ')' expected (to close '(' at line 6) near '123'
    let l:pattern = '\v^.*:(\d+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'type': 'E',
        \   'text': l:match[2],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('lua', {
\   'name': 'luac',
\   'executable_callback': 'ale_linters#lua#luac#GetExecutable',
\   'command_callback': 'ale_linters#lua#luac#GetCommand',
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#lua#luac#Handle',
\})
