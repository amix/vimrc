function! ale_linters#yaml#circleci#Handle(buffer, lines) abort
    let l:match_index = -1
    let l:output = []

    for l:index in range(len(a:lines))
        let l:line = a:lines[l:index]

        if l:line =~? 'Error: ERROR IN CONFIG FILE:'
            let l:match_index = l:index + 1
            break
        endif
    endfor

    if l:match_index > 0
        return [{
        \   'type': 'E',
        \   'lnum': 1,
        \   'text': a:lines[l:match_index],
        \   'detail': join(a:lines[l:match_index :], "\n"),
        \}]
    endif

    return []
endfunction

" The circleci validate requires network requests, so we'll only run it when
" files are saved to prevent the server from being hammered.
call ale#linter#Define('yaml', {
\   'name': 'circleci',
\   'executable': {b -> expand('#' . b . ':p') =~? '\.circleci' ? 'circleci' : ''},
\   'command': 'circleci config validate - < %s',
\   'callback': 'ale_linters#yaml#circleci#Handle',
\   'output_stream': 'stderr',
\   'lint_file': 1,
\})
