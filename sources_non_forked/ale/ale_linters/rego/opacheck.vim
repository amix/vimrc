" Description: opa check for rego files

call ale#Set('rego_opacheck_executable', 'opa')
call ale#Set('rego_opacheck_options', '')

function! ale_linters#rego#opacheck#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'rego_opacheck_executable')
endfunction

function! ale_linters#rego#opacheck#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'rego_opacheck_options')

    return ale#Escape(ale_linters#rego#opacheck#GetExecutable(a:buffer))
    \   . ' check %s --format json '
    \   . (!empty(l:options) ? ' ' . l:options : '')
endfunction

function! ale_linters#rego#opacheck#Handle(buffer, lines) abort
    let l:output = []

    let l:errors = ale#util#FuzzyJSONDecode(a:lines, {'errors': []})
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:file = expand('#' . a:buffer . ':p')

    for l:error in l:errors['errors']
        if has_key(l:error, 'location')
            call add(l:output, {
            \   'filename': ale#path#GetAbsPath(l:dir, l:error['location']['file']),
            \   'lnum': l:error['location']['row'],
            \   'col': l:error['location']['col'],
            \   'text': l:error['message'],
            \   'code': l:error['code'],
            \   'type': 'E',
            \})
        else
            call add(l:output, {
            \   'filename': l:file,
            \   'lnum': 0,
            \   'col': 0,
            \   'text': l:error['message'],
            \   'code': l:error['code'],
            \   'type': 'E',
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('rego', {
\   'name': 'opacheck',
\   'output_stream': 'both',
\   'executable': function('ale_linters#rego#opacheck#GetExecutable'),
\   'command': function('ale_linters#rego#opacheck#GetCommand'),
\   'callback': 'ale_linters#rego#opacheck#Handle',
\})
