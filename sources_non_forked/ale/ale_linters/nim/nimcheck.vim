" Author: Baabelfish
" Description: Typechecking for nim files

let s:end_col_patterns = [
\   '\v''([^'']+)'' is declared but not used.*',
\   '\videntifier expected, but found ''([^'']+)''',
\   '\vimported and not used: ''([^'']+)''.*',
\   '\vundeclared identifier: ''([^'']+)''',
\   '\v''([^'']+)'' cannot be assigned to',
\   '\vredefinition of ''([^'']+)'';',
\]

function! ale_linters#nim#nimcheck#Handle(buffer, lines) abort
    let l:buffer_filename = fnamemodify(bufname(a:buffer), ':p:t')
    let l:pattern = '^\(.\+\.nim\)(\(\d\+\), \(\d\+\)) \(.\+\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        " Only show errors of the current buffer
        " NOTE: Checking filename only is OK because nim enforces unique
        "       module names.
        let l:temp_buffer_filename = fnamemodify(l:match[1], ':p:t')

        if l:buffer_filename isnot# '' && l:temp_buffer_filename isnot# l:buffer_filename
            continue
        endif

        let l:item = {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': 'W',
        \}

        " Extract error type from message of type 'Error: Some error message'
        let l:error_match = matchlist(l:item.text, '^\(.\{-}\): \(.\+\)$')

        if !empty(l:error_match)
            if l:error_match[1] is# 'Error'
                let l:item.type = 'E'
                let l:item.text = l:error_match[2]
            elseif l:error_match[1] is# 'Warning'
            \|| l:error_match[1] is# 'Hint'
                let l:item.text = l:error_match[2]
            endif
        endif

        let l:code_match = matchlist(l:item.text, '\v^(.+) \[([^ \[]+)\]$')

        if !empty(l:code_match)
            let l:item.text = l:code_match[1]
            let l:item.code = l:code_match[2]
        endif

        " Find position end_col.
        for l:col_match in ale#util#GetMatches(l:item.text, s:end_col_patterns)
            let l:item.end_col = l:item.col + len(l:col_match[1]) - 1
        endfor

        call add(l:output, l:item)
    endfor

    return l:output
endfunction


function! ale_linters#nim#nimcheck#GetCommand(buffer) abort
    return 'nim check --verbosity:0 --colors:off --listFullPaths %s'
endfunction


call ale#linter#Define('nim', {
\    'name': 'nimcheck',
\    'executable': 'nim',
\    'output_stream': 'both',
\    'command': function('ale_linters#nim#nimcheck#GetCommand'),
\    'callback': 'ale_linters#nim#nimcheck#Handle',
\    'lint_file': 1,
\})
