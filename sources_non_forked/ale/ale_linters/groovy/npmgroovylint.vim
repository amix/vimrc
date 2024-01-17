" Author: lucas-str <lucas.sturelle@ik.me>
" Description: Integration of npm-groovy-lint for Groovy files.

call ale#Set('groovy_npmgroovylint_executable', 'npm-groovy-lint')
call ale#Set('groovy_npmgroovylint_options', '--loglevel warning')

function! ale_linters#groovy#npmgroovylint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'groovy_npmgroovylint_options')

    return '%e --failon none --output json'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

function! ale_linters#groovy#npmgroovylint#Handle(buffer, lines) abort
    let l:output = []
    let l:json = ale#util#FuzzyJSONDecode(a:lines, {})

    for [l:filename, l:file] in items(get(l:json, 'files', {}))
        for l:error in get(l:file, 'errors', [])
            let l:output_line = {
            \   'filename': l:filename,
            \   'lnum': l:error.line,
            \   'text': l:error.msg,
            \   'type': toupper(l:error.severity[0]),
            \}

            if has_key(l:error, 'range')
                let l:output_line.col = l:error.range.start.character
                let l:output_line.end_col = l:error.range.end.character
                let l:output_line.end_lnum = l:error.range.end.line
            endif

            call add(l:output, l:output_line)
        endfor
    endfor

    return l:output
endfunction

call ale#linter#Define('groovy', {
\   'name': 'npm-groovy-lint',
\   'executable': {b -> ale#Var(b, 'groovy_npmgroovylint_executable')},
\   'command': function('ale_linters#groovy#npmgroovylint#GetCommand'),
\   'callback': 'ale_linters#groovy#npmgroovylint#Handle',
\})
