" Authors: Klaas Pieter Annema <https://github.com/klaaspieter>, bosr <bosr@bosr.cc>
" Description: Support for swift-format https://github.com/apple/swift-format

function! ale_linters#swift#appleswiftformat#GetLinterCommand(buffer) abort
    let l:command_args = ale#swift#GetAppleSwiftFormatCommand(a:buffer) . ' lint %t'
    let l:config_args = ale#swift#GetAppleSwiftFormatConfigArgs(a:buffer)

    if l:config_args isnot# ''
        let l:command_args = l:command_args . ' ' . l:config_args
    endif

    return l:command_args
endfunction

function! ale_linters#swift#appleswiftformat#Handle(buffer, lines) abort
    " Matches the typical output of swift-format, that is lines of the following pattern:
    "
    " Sources/main.swift:4:21: warning: [DoNotUseSemicolons] remove ';' and move the next statement to the new line
    " Sources/main.swift:3:12: warning: [Spacing] remove 1 space
    let l:pattern = '\v^.*:(\d+):(\d+): (\S+): \[(\S+)\] (.*)$'
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
\   'name': 'apple-swift-format',
\   'executable': function('ale#swift#GetAppleSwiftFormatExecutable'),
\   'command': function('ale_linters#swift#appleswiftformat#GetLinterCommand'),
\   'output_stream': 'stderr',
\   'language': 'swift',
\   'callback': 'ale_linters#swift#appleswiftformat#Handle'
\})
