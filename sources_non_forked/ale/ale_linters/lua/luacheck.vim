" Author: Sol Bekic https://github.com/s-ol
" Description: luacheck linter for lua files

let g:ale_lua_luacheck_executable =
\   get(g:, 'ale_lua_luacheck_executable', 'luacheck')

let g:ale_lua_luacheck_options =
\   get(g:, 'ale_lua_luacheck_options', '')

function! ale_linters#lua#luacheck#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'lua_luacheck_executable')
endfunction

function! ale_linters#lua#luacheck#GetCommand(buffer) abort
    return ale#Escape(ale_linters#lua#luacheck#GetExecutable(a:buffer))
    \   . ' ' . ale#Var(a:buffer, 'lua_luacheck_options')
    \   . ' --formatter plain --codes --filename %s -'
endfunction

function! ale_linters#lua#luacheck#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " artal.lua:159:17: (W111) shadowing definition of loop variable 'i' on line 106
    " artal.lua:182:7: (W213) unused loop variable 'i'
    let l:pattern = '^.*:\(\d\+\):\(\d\+\): (\([WE]\)\(\d\+\)) \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
        \   && l:match[3] is# 'W'
        \   && index(range(611, 614), str2nr(l:match[4])) >= 0
            continue
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3],
        \   'code': l:match[3] . l:match[4],
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('lua', {
\   'name': 'luacheck',
\   'executable_callback': 'ale_linters#lua#luacheck#GetExecutable',
\   'command_callback': 'ale_linters#lua#luacheck#GetCommand',
\   'callback': 'ale_linters#lua#luacheck#Handle',
\})
