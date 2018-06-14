" Author: Eddie Lebow https://github.com/elebow
" Description: Cucumber, a BDD test tool

function! ale_linters#cucumber#cucumber#GetCommand(buffer) abort
    let l:features_dir = ale#path#FindNearestDirectory(a:buffer, 'features')

    if !empty(l:features_dir)
        let l:features_arg = '-r ' . ale#Escape(l:features_dir)
    else
        let l:features_arg = ''
    endif

    return 'cucumber --dry-run --quiet --strict --format=json '
    \   . l:features_arg . ' %t'
endfunction

function! ale_linters#cucumber#cucumber#Handle(buffer, lines) abort
    try
        let l:json = ale#util#FuzzyJSONDecode(a:lines, {})[0]
    catch
        return []
    endtry

    let l:output = []
    for l:element in get(l:json, 'elements', [])
        for l:step in l:element['steps']
            if l:step['result']['status'] is# 'undefined'
                call add(l:output, {
                \   'lnum': l:step['line'],
                \   'code': 'E',
                \   'text': 'Undefined step'
                \})
            endif
        endfor
    endfor

    return l:output
endfunction

call ale#linter#Define('cucumber', {
\   'name': 'cucumber',
\   'executable': 'cucumber',
\   'command_callback': 'ale_linters#cucumber#cucumber#GetCommand',
\   'callback': 'ale_linters#cucumber#cucumber#Handle'
\})
