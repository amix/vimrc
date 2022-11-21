" Author: Jose Soto <jose@tighten.co>
"
" Description: Tighten Opinionated PHP Linting
" Website: https://github.com/tightenco/tlint

call ale#Set('php_tlint_executable', 'tlint')
call ale#Set('php_tlint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('php_tlint_options', '')

function! ale_linters#php#tlint#GetProjectRoot(buffer) abort
    let l:composer_path = ale#path#FindNearestFile(a:buffer, 'composer.json')

    if !empty(l:composer_path)
        return fnamemodify(l:composer_path, ':h')
    endif

    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

function! ale_linters#php#tlint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'php_tlint', [
    \   'vendor/bin/tlint',
    \   'tlint',
    \])
endfunction

function! ale_linters#php#tlint#GetCommand(buffer) abort
    let l:executable = ale_linters#php#tlint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'php_tlint_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' lint %s'
endfunction

function! ale_linters#php#tlint#Handle(buffer, lines) abort
    " Matches against lines like the following:
    "
    " ! There should be 1 space around `.` concatenations, and additional lines should always start with a `.`
    " 22 : `        $something = 'a'.'name';`
    "
    let l:loop_count = 0
    let l:messages_pattern = '^\! \(.*\)'
    let l:output = []
    let l:pattern = '^\(\d\+\) \:'
    let l:temp_messages = []

    for l:message in ale#util#GetMatches(a:lines, l:messages_pattern)
        call add(l:temp_messages, l:message)
    endfor

    let l:loop_count = 0

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:num = l:match[1]
        let l:text = l:temp_messages[l:loop_count]

        call add(l:output, {
        \   'lnum': l:num,
        \   'col': 0,
        \   'text': l:text,
        \   'type': 'W',
        \   'sub_type': 'style',
        \})

        let l:loop_count += 1
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'tlint',
\   'executable': function('ale_linters#php#tlint#GetExecutable'),
\   'command': function('ale_linters#php#tlint#GetCommand'),
\   'callback': 'ale_linters#php#tlint#Handle',
\   'project_root': function('ale_linters#php#tlint#GetProjectRoot'),
\})
