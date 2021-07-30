" Author: hauleth - https://github.com/hauleth

" always, yes, never
call ale#Set('dockerfile_hadolint_use_docker', 'never')
call ale#Set('dockerfile_hadolint_docker_image', 'hadolint/hadolint')

function! ale_linters#dockerfile#hadolint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " -:19 DL3001 warning: Pipe chain should start with a raw value.
    " /dev/stdin:19:3 unexpected thing
    let l:pattern = '\v^%(/dev/stdin|-):(\d+):?(\d+)? ((DL|SC)(\d+) )?((.+)?: )?(.+)$'
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

        " Shellcheck knows a 'style' severity - pin it to info level as well.
        if l:match[7] is# 'style'
            let l:type = 'I'
        elseif l:match[7] is# 'info'
            let l:type = 'I'
        elseif l:match[7] is# 'warning'
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        let l:text = l:match[8]
        let l:detail = l:match[8]
        let l:domain = 'https://github.com/hadolint/hadolint/wiki/'
        let l:code = ''
        let l:link = ''

        if l:match[4] is# 'SC'
            let l:domain = 'https://github.com/koalaman/shellcheck/wiki/'
        endif

        if l:match[5] isnot# ''
            let l:code = l:match[4] . l:match[5]
            let l:link = ' ( ' . l:domain . l:code . ' )'
            let l:text = l:code . ': ' . l:detail
            let l:detail = l:code . l:link . "\n\n" . l:detail
        else
            let l:type = 'E'
            let l:detail = 'hadolint could not parse the file because of a syntax error.'
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
    let l:opts = '--no-color -'

    if l:command is# 'docker'
        return printf('docker run --rm -i %s hadolint %s',
        \ ale#Var(a:buffer, 'dockerfile_hadolint_docker_image'),
        \ l:opts)
    endif

    return 'hadolint ' . l:opts
endfunction


call ale#linter#Define('dockerfile', {
\   'name': 'hadolint',
\   'executable': function('ale_linters#dockerfile#hadolint#GetExecutable'),
\   'command': function('ale_linters#dockerfile#hadolint#GetCommand'),
\   'callback': 'ale_linters#dockerfile#hadolint#Handle',
\})
