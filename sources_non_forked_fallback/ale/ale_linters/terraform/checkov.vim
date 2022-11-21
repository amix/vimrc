" Author: Thyme-87 <thyme-87@posteo.me>
" Description: use checkov for providing warnings via ale

call ale#Set('terraform_checkov_executable', 'checkov')
call ale#Set('terraform_checkov_options', '')

function! ale_linters#terraform#checkov#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'terraform_checkov_executable')
endfunction

function! ale_linters#terraform#checkov#GetCommand(buffer) abort
    return '%e ' . '-f %t -o json --quiet ' . ale#Var(a:buffer, 'terraform_checkov_options')
endfunction

function! ale_linters#terraform#checkov#Handle(buffer, lines) abort
    let l:output = []

    let l:results = get(get(ale#util#FuzzyJSONDecode(a:lines, {}), 'results', []), 'failed_checks', [])

    for l:violation in l:results
        call add(l:output, {
        \   'filename': l:violation['file_path'],
        \   'lnum': l:violation['file_line_range'][0],
        \   'end_lnum': l:violation['file_line_range'][1],
        \   'text': l:violation['check_name'] . ' [' . l:violation['check_id'] . ']',
        \   'detail': l:violation['check_id'] . ': ' . l:violation['check_name'] . "\n" .
        \             'For more information, see: '. l:violation['guideline'],
        \   'type': 'W',
        \   })
    endfor

    return l:output
endfunction

call ale#linter#Define('terraform', {
\   'name': 'checkov',
\   'output_stream': 'stdout',
\   'executable': function('ale_linters#terraform#checkov#GetExecutable'),
\   'command': function('ale_linters#terraform#checkov#GetCommand'),
\   'callback': 'ale_linters#terraform#checkov#Handle',
\})
