function! ale#handlers#actionlint#GetCommand(buffer) abort
    return '%e --no-color --oneline %t'
endfunction

function! ale#handlers#actionlint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    ".github/workflows/main.yml:19:0: could not parse as YAML: yaml: line 19: mapping values are not allowed in this context [yaml-syntax]
    let l:pattern = '\v^.*:(\d+):(\d+): (.+) \[(.+)\]$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \   'code': l:match[4],
        \   'type': 'E',
        \}

        call add(l:output, l:item)
    endfor

    return l:output
endfunction
