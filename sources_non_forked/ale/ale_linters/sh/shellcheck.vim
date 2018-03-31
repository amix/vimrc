" Author: w0rp <devw0rp@gmail.com>
" Description: This file adds support for using the shellcheck linter with
"   shell scripts.

" This global variable can be set with a string of comma-separated error
" codes to exclude from shellcheck. For example:
"
" let g:ale_sh_shellcheck_exclusions = 'SC2002,SC2004'
let g:ale_sh_shellcheck_exclusions =
\   get(g:, 'ale_sh_shellcheck_exclusions', get(g:, 'ale_linters_sh_shellcheck_exclusions', ''))

let g:ale_sh_shellcheck_executable =
\   get(g:, 'ale_sh_shellcheck_executable', 'shellcheck')

let g:ale_sh_shellcheck_options =
\   get(g:, 'ale_sh_shellcheck_options', '')

function! ale_linters#sh#shellcheck#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'sh_shellcheck_executable')
endfunction

function! ale_linters#sh#shellcheck#GetDialectArgument(buffer) abort
    let l:shell_type = ale#handlers#sh#GetShellType(a:buffer)

    if !empty(l:shell_type)
        " Use the dash dialect for /bin/ash, etc.
        if l:shell_type is# 'ash'
            return 'dash'
        endif

        return l:shell_type
    endif

    " If there's no hashbang, try using Vim's buffer variables.
    if getbufvar(a:buffer, 'is_bash', 0)
        return 'bash'
    elseif getbufvar(a:buffer, 'is_sh', 0)
        return 'sh'
    elseif getbufvar(a:buffer, 'is_kornshell', 0)
        return 'ksh'
    endif

    return ''
endfunction

function! ale_linters#sh#shellcheck#VersionCheck(buffer) abort
    let l:executable = ale_linters#sh#shellcheck#GetExecutable(a:buffer)

    " Don't check the version again if we've already cached it.
    return !ale#semver#HasVersion(l:executable)
    \   ? ale#Escape(l:executable) . ' --version'
    \   : ''
endfunction

function! ale_linters#sh#shellcheck#GetCommand(buffer, version_output) abort
    let l:executable = ale_linters#sh#shellcheck#GetExecutable(a:buffer)
    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    let l:options = ale#Var(a:buffer, 'sh_shellcheck_options')
    let l:exclude_option = ale#Var(a:buffer, 'sh_shellcheck_exclusions')
    let l:dialect = ale_linters#sh#shellcheck#GetDialectArgument(a:buffer)
    let l:external_option = ale#semver#GTE(l:version, [0, 4, 0]) ? ' -x' : ''

    return ale#path#BufferCdString(a:buffer)
    \   . ale#Escape(l:executable)
    \   . (!empty(l:dialect) ? ' -s ' . l:dialect : '')
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . (!empty(l:exclude_option) ? ' -e ' . l:exclude_option : '')
    \   . l:external_option
    \   . ' -f gcc -'
endfunction

function! ale_linters#sh#shellcheck#Handle(buffer, lines) abort
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):(\d+)?:? ([^:]+): (.+) \[([^\]]+)\]$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if l:match[4] is# 'error'
            let l:type = 'E'
        elseif l:match[4] is# 'note'
            let l:type = 'I'
        else
            let l:type = 'W'
        endif

        let l:item = {
        \   'lnum': str2nr(l:match[2]),
        \   'type': l:type,
        \   'text': l:match[5],
        \   'code': l:match[6],
        \}

        if !empty(l:match[3])
            let l:item.col = str2nr(l:match[3])
        endif

        " If the filename is something like <stdin>, <nofile> or -, then
        " this is an error for the file we checked.
        if l:match[1] isnot# '-' && l:match[1][0] isnot# '<'
            let l:item['filename'] = l:match[1]
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('sh', {
\   'name': 'shellcheck',
\   'executable_callback': 'ale_linters#sh#shellcheck#GetExecutable',
\   'command_chain': [
\       {'callback': 'ale_linters#sh#shellcheck#VersionCheck'},
\       {'callback': 'ale_linters#sh#shellcheck#GetCommand'},
\   ],
\   'callback': 'ale_linters#sh#shellcheck#Handle',
\})
