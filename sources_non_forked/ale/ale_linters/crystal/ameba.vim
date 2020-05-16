" Author: Harrison Bachrach - https://github.com/HarrisonB
" Description: Ameba, a linter for crystal files

call ale#Set('crystal_ameba_executable', 'bin/ameba')

function! ale_linters#crystal#ameba#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'crystal_ameba_executable')

    return ale#Escape(l:executable)
    \   . ' --format json '
    \   .  ale#Escape(expand('#' . a:buffer . ':p'))
endfunction

" Handle output from ameba
function! ale_linters#crystal#ameba#HandleAmebaOutput(buffer, lines) abort
    if len(a:lines) == 0
        return []
    endif

    let l:errors = ale#util#FuzzyJSONDecode(a:lines[0], {})

    if !has_key(l:errors, 'summary')
    \|| l:errors['summary']['issues_count'] == 0
    \|| empty(l:errors['sources'])
        return []
    endif

    let l:output = []

    for l:error in l:errors['sources'][0]['issues']
        let l:start_col = str2nr(l:error['location']['column'])
        let l:end_col = str2nr(l:error['end_location']['column'])

        if !l:end_col
            let l:end_col = l:start_col + 1
        endif

        call add(l:output, {
        \   'lnum': str2nr(l:error['location']['line']),
        \   'col': l:start_col,
        \   'end_col': l:end_col,
        \   'code': l:error['rule_name'],
        \   'text': l:error['message'],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('crystal', {
\   'name': 'ameba',
\   'executable': {b -> ale#Var(b, 'crystal_ameba_executable')},
\   'command': function('ale_linters#crystal#ameba#GetCommand'),
\   'callback': 'ale_linters#crystal#ameba#HandleAmebaOutput',
\   'lint_file': 1,
\})
