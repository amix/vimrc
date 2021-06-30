" Author: Keith Maxwell <keith.maxwell@gmail.com>
" Description: terraform fmt to check for errors

call ale#Set('terraform_terraform_executable', 'terraform')

function! ale_linters#terraform#terraform#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'terraform_terraform_executable')
endfunction

function! ale_linters#terraform#terraform#GetCommand(buffer) abort
    return ale#Escape(ale_linters#terraform#terraform#GetExecutable(a:buffer))
    \   . ' validate -no-color -json '
endfunction

function! ale_linters#terraform#terraform#GetType(severity) abort
    if a:severity is? 'warning'
        return 'W'
    endif

    return 'E'
<<<<<<< HEAD
=======
endfunction

function! ale_linters#terraform#terraform#GetDetail(error) abort
    return get(a:error, 'detail', get(a:error, 'summary', ''))
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
endfunction

function! ale_linters#terraform#terraform#Handle(buffer, lines) abort
    let l:output = []

    let l:errors = ale#util#FuzzyJSONDecode(a:lines, {'diagnostics': []})
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:file = expand('#' . a:buffer . ':p')

    for l:error in l:errors['diagnostics']
        if has_key(l:error, 'range')
            call add(l:output, {
            \   'filename': ale#path#GetAbsPath(l:dir, l:error['range']['filename']),
            \   'lnum': l:error['range']['start']['line'],
            \   'col': l:error['range']['start']['column'],
<<<<<<< HEAD
            \   'text': l:error['detail'],
=======
            \   'text': ale_linters#terraform#terraform#GetDetail(l:error),
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
            \   'type': ale_linters#terraform#terraform#GetType(l:error['severity']),
            \})
        else
            call add(l:output, {
            \   'filename': l:file,
            \   'lnum': 0,
            \   'col': 0,
<<<<<<< HEAD
            \   'text': l:error['detail'],
=======
            \   'text': ale_linters#terraform#terraform#GetDetail(l:error),
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
            \   'type': ale_linters#terraform#terraform#GetType(l:error['severity']),
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('terraform', {
\   'name': 'terraform',
\   'output_stream': 'stdout',
\   'executable': function('ale_linters#terraform#terraform#GetExecutable'),
\   'command': function('ale_linters#terraform#terraform#GetCommand'),
\   'callback': 'ale_linters#terraform#terraform#Handle',
\})
