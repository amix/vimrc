" Author: Niraj Thapaliya - https://github.com/nthapaliya
" Description: Lints fish files using fish -n

function! ale_linters#fish#fish#Handle(buffer, lines) abort
    " Matches patterns such as:
    "
    " home/.config/fish/functions/foo.fish (line 1): Missing end to balance this function definition
    " function foo
    " ^
    "
    " OR, patterns such as:
    "
    " Unsupported use of '||'. In fish, please use 'COMMAND; or COMMAND'.
    " /tmp/vLz620o/258/test.fish (line 2): if set -q SSH_CLIENT || set -q SSH_TTY
    "                                                            ^
    "
    " fish -n can return errors in either format.
    let l:pattern = '^\(.* (line \(\d\+\)): \)\(.*\)$'
    let l:column_pattern = '^ *\^'
    let l:output = []
    let l:column_offset = 0
    let l:last_line_with_message = ''

    for l:line in a:lines
        " Look for error lines first.
        let l:match = matchlist(l:line, l:pattern)

        if !empty(l:match)
            if !empty(l:last_line_with_message)
                let l:text = l:last_line_with_message
            else
                let l:text = l:match[3]
            endif

            let l:column_offset = len(l:match[1])

            let l:last_line_with_message = ''
            call add(l:output, {
            \  'col': 0,
            \  'lnum': str2nr(l:match[2]),
            \  'text': l:text,
            \})
        else
            " Look for column markers like '   ^' second.
            " The column index will be set according to how long the line is.
            let l:column_match = matchstr(l:line, l:column_pattern)

            if !empty(l:column_match) && !empty(l:output)
                let l:output[-1].col = len(l:column_match) - l:column_offset
                let l:last_line_with_message = ''
            else
                let l:last_line_with_message = l:line
                let l:column_offset = 0
            endif
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('fish', {
\   'name': 'fish',
\   'output_stream': 'stderr',
\   'executable': 'fish',
\   'command': 'fish -n %t',
\   'callback': 'ale_linters#fish#fish#Handle',
\})
