function! ale#handlers#actionlint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'yaml_actionlint_options')

    " automatically add --no-color option if not defined
    if l:options !~# '--no-color'
        let l:options .= ' --no-color'
    endif

    " automatically add --oneline option if not defined
    if l:options !~# '--oneline'
        let l:options .= ' --oneline'
    endif

    return '%e ' . l:options . ' %t'
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
