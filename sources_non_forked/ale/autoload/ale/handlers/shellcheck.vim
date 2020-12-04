" Author: w0rp <devw0rp@gmail.com>
" Description: This file adds support for using the shellcheck linter

" Shellcheck supports shell directives to define the shell dialect for scripts
" that do not have a shebang for some reason.
" https://github.com/koalaman/shellcheck/wiki/Directive#shell
function! ale#handlers#shellcheck#GetShellcheckDialectDirective(buffer) abort
    let l:linenr = 0
    let l:pattern = '\s\{-}#\s\{-}shellcheck\s\{-}shell=\(.*\)'
    let l:possible_shell = ['bash', 'dash', 'ash', 'tcsh', 'csh', 'zsh', 'ksh', 'sh']

    while l:linenr < min([50, line('$')])
        let l:linenr += 1
        let l:match = matchlist(getline(l:linenr), l:pattern)

        if len(l:match) > 1 && index(l:possible_shell, l:match[1]) >= 0
            return l:match[1]
        endif
    endwhile

    return ''
endfunction

function! ale#handlers#shellcheck#GetDialectArgument(buffer) abort
    let l:shell_type = ale#handlers#shellcheck#GetShellcheckDialectDirective(a:buffer)

    if empty(l:shell_type)
        let l:shell_type = ale#handlers#sh#GetShellType(a:buffer)
    endif

    if !empty(l:shell_type)
        " Use the dash dialect for /bin/ash, etc.
        if l:shell_type is# 'ash'
            return 'dash'
        endif

        return l:shell_type
    endif

    return ''
endfunction

function! ale#handlers#shellcheck#GetCommand(buffer, version) abort
    let l:options = ale#Var(a:buffer, 'sh_shellcheck_options')
    let l:exclude_option = ale#Var(a:buffer, 'sh_shellcheck_exclusions')
    let l:dialect = ale#Var(a:buffer, 'sh_shellcheck_dialect')
    let l:external_option = ale#semver#GTE(a:version, [0, 4, 0]) ? ' -x' : ''
    let l:cd_string = ale#Var(a:buffer, 'sh_shellcheck_change_directory')
    \   ? ale#path#BufferCdString(a:buffer)
    \   : ''

    if l:dialect is# 'auto'
        let l:dialect = ale#handlers#shellcheck#GetDialectArgument(a:buffer)
    endif

    return l:cd_string
    \   . '%e'
    \   . (!empty(l:dialect) ? ' -s ' . l:dialect : '')
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . (!empty(l:exclude_option) ? ' -e ' . l:exclude_option : '')
    \   . l:external_option
    \   . ' -f gcc -'
endfunction

function! ale#handlers#shellcheck#Handle(buffer, lines) abort
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

function! ale#handlers#shellcheck#DefineLinter(filetype) abort
    " This global variable can be set with a string of comma-separated error
    " codes to exclude from shellcheck. For example:
    " let g:ale_sh_shellcheck_exclusions = 'SC2002,SC2004'
    call ale#Set('sh_shellcheck_exclusions', get(g:, 'ale_linters_sh_shellcheck_exclusions', ''))
    call ale#Set('sh_shellcheck_executable', 'shellcheck')
    call ale#Set('sh_shellcheck_dialect', 'auto')
    call ale#Set('sh_shellcheck_options', '')
    call ale#Set('sh_shellcheck_change_directory', 1)

    call ale#linter#Define(a:filetype, {
    \   'name': 'shellcheck',
    \   'executable': {buffer -> ale#Var(buffer, 'sh_shellcheck_executable')},
    \   'command': {buffer -> ale#semver#RunWithVersionCheck(
    \       buffer,
    \       ale#Var(buffer, 'sh_shellcheck_executable'),
    \       '%e --version',
    \       function('ale#handlers#shellcheck#GetCommand'),
    \   )},
    \   'callback': 'ale#handlers#shellcheck#Handle',
    \})
endfunction
