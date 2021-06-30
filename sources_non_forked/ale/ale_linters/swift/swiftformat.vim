" Author: Klaas Pieter Annema <https://github.com/klaaspieter>
" Description: Support for swift-format https://github.com/apple/swift-format

let s:default_executable = 'swift-format'
call ale#Set('swift_swiftformat_executable', s:default_executable)

function! ale_linters#swift#swiftformat#UseSwift(buffer) abort
    let l:swift_config = ale#path#FindNearestFile(a:buffer, 'Package.swift')
    let l:executable = ale#Var(a:buffer, 'swift_swiftformat_executable')

    return !empty(l:swift_config) && l:executable is# s:default_executable
endfunction

function! ale_linters#swift#swiftformat#GetExecutable(buffer) abort
    if ale_linters#swift#swiftformat#UseSwift(a:buffer)
        return 'swift'
    endif

    return ale#Var(a:buffer, 'swift_swiftformat_executable')
endfunction

function! ale_linters#swift#swiftformat#GetCommand(buffer) abort
    let l:executable = ale_linters#swift#swiftformat#GetExecutable(a:buffer)
    let l:args = '--mode lint %t'

    if ale_linters#swift#swiftformat#UseSwift(a:buffer)
        let l:args = 'run swift-format' . ' ' . l:args
    endif

    return ale#Escape(l:executable) . ' ' . l:args
endfunction

function! ale_linters#swift#swiftformat#Handle(buffer, lines) abort
    " Matches lines of the following pattern:
    "
    " Sources/main.swift:4:21: warning: [DoNotUseSemicolons]: remove ';' and move the next statement to the new line
    " Sources/main.swift:3:12: warning: [Spacing]: remove 1 space
    let l:pattern = '\v^.*:(\d+):(\d+): (\S+) \[(\S+)\]: (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3] is# 'error' ? 'E' : 'W',
        \   'code': l:match[4],
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction


call ale#linter#Define('swift', {
\   'name': 'swift-format',
\   'executable': function('ale_linters#swift#swiftformat#GetExecutable'),
\   'command': function('ale_linters#swift#swiftformat#GetCommand'),
\   'output_stream': 'stderr',
\   'language': 'swift',
\   'callback': 'ale_linters#swift#swiftformat#Handle'
\})
