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
endfunction

function! ale_linters#terraform#terraform#GetDetail(error) abort
    let l:detail = get(a:error, 'detail', '')

    if strlen(l:detail) > 0
        return l:detail
    else
        return get(a:error, 'summary', '')
    endif
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
            \   'text': ale_linters#terraform#terraform#GetDetail(l:error),
            \   'type': ale_linters#terraform#terraform#GetType(l:error['severity']),
            \})
        else
            call add(l:output, {
            \   'filename': l:file,
            \   'lnum': 0,
            \   'col': 0,
            \   'text': ale_linters#terraform#terraform#GetDetail(l:error),
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
