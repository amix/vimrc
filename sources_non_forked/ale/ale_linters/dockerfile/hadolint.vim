" Author: hauleth - https://github.com/hauleth

" always, yes, never
call ale#Set('dockerfile_hadolint_use_docker', 'never')
call ale#Set('dockerfile_hadolint_docker_image', 'hadolint/hadolint')

function! ale_linters#dockerfile#hadolint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " /dev/stdin:19 DL3001 Pipe chain should start with a raw value.
    " /dev/stdin:19:3 unexpected thing
    let l:pattern = '\v^/dev/stdin:(\d+):?(\d+)? ((DL|SC)(\d+) )?(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:lnum = 0
        let l:colnum = 0

        if l:match[1] isnot# ''
            let l:lnum = l:match[1] + 0
        endif

        if l:match[2] isnot# ''
            let l:colnum = l:match[2] + 0
        endif

        let l:type = 'W'
        let l:text = l:match[6]
        let l:detail = l:match[6]
        let l:domain = 'https://github.com/hadolint/hadolint/wiki/'

        if l:match[4] is# 'SC'
            let l:domain = 'https://github.com/koalaman/shellcheck/wiki/'
        endif

        if l:match[5] isnot# ''
            let l:code = l:match[4] . l:match[5]
            let l:link = ' ( ' . l:domain . l:code . ' )'
            let l:detail = l:code . l:link . "\n\n" . l:detail
        else
            let l:type = 'E'
        endif

        call add(l:output, {
        \   'lnum': l:lnum,
        \   'col': l:colnum,
        \   'type': l:type,
        \   'text': l:text,
        \   'detail': l:detail
        \})
    endfor

    return l:output
endfunction

" This is a little different than the typical 'executable' callback.  We want
" to afford the user the chance to say always use docker, never use docker,
" and use docker if the hadolint executable is not present on the system.
"
" In the case of neither docker nor hadolint executables being present, it
" really doesn't matter which we return -- either will have the effect of
" 'nope, can't use this linter!'.

function! ale_linters#dockerfile#hadolint#GetExecutable(buffer) abort
    let l:use_docker = ale#Var(a:buffer, 'dockerfile_hadolint_use_docker')

    " check for mandatory directives
    if l:use_docker is# 'never'
        return 'hadolint'
    elseif l:use_docker is# 'always'
        return 'docker'
    endif

    " if we reach here, we want to use 'hadolint' if present...
    if executable('hadolint')
        return 'hadolint'
    endif

    "... and 'docker' as a fallback.
    return 'docker'
endfunction

function! ale_linters#dockerfile#hadolint#GetCommand(buffer) abort
    let l:command = ale_linters#dockerfile#hadolint#GetExecutable(a:buffer)

    if l:command is# 'docker'
        return 'docker run --rm -i ' . ale#Var(a:buffer, 'dockerfile_hadolint_docker_image')
    endif

    return 'hadolint -'
endfunction


call ale#linter#Define('dockerfile', {
\   'name': 'hadolint',
\   'executable_callback': 'ale_linters#dockerfile#hadolint#GetExecutable',
\   'command_callback': 'ale_linters#dockerfile#hadolint#GetCommand',
\   'callback': 'ale_linters#dockerfile#hadolint#Handle',
\})
