" Author: Sol Bekic https://github.com/s-ol
" Description: luacheck linter for lua files

call ale#Set('lua_luacheck_executable', 'luacheck')
call ale#Set('lua_luacheck_options', '')

function! s:IsInRuntimepath(buffer) abort
    let l:runtimepath_dirs = split(&runtimepath, ',')

    for l:dir in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:runtime_dir in l:runtimepath_dirs
            if l:dir is# l:runtime_dir
                return 1
            endif
        endfor
    endfor

    return 0
endfunction

function! ale_linters#lua#luacheck#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'lua_luacheck_options')

    " Add `--globals vim` by default if the file is in runtimepath.
    if l:options !~# '--globals'
        let l:in_runtime = getbufvar(a:buffer, 'ale_in_runtimepath', v:null)

        if l:in_runtime is v:null
            let l:in_runtime = s:IsInRuntimepath(a:buffer)
            " Save the result of check this buffer so we only check once.
            call setbufvar(a:buffer, 'ale_in_runtimepath', l:in_runtime)
        endif

        if l:in_runtime
            if !empty(l:options)
                let l:options .= ' '
            endif

            let l:options .= '--globals vim'
        endif
    endif

    return '%e' . ale#Pad(l:options)
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
\   'executable': {b -> ale#Var(b, 'lua_luacheck_executable')},
\   'command': function('ale_linters#lua#luacheck#GetCommand'),
\   'callback': 'ale_linters#lua#luacheck#Handle',
\})
