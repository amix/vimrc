" Author: Alexander Olofsson <alexander.olofsson@liu.se>

call ale#Set('dockerfile_dockerfile_lint_executable', 'dockerfile_lint')
call ale#Set('dockerfile_dockerfile_lint_options', '')

function! ale_linters#dockerfile#dockerfile_lint#GetType(type) abort
    if a:type is? 'error'
        return 'E'
    elseif a:type is? 'warn'
        return 'W'
    endif

    return 'I'
endfunction

function! ale_linters#dockerfile#dockerfile_lint#Handle(buffer, lines) abort
    try
        let l:data = json_decode(join(a:lines, ''))
    catch
        return []
    endtry

    if empty(l:data)
        " Should never happen, but it's better to be on the safe side
        return []
    endif

    let l:messages = []

    for l:type in ['error', 'warn', 'info']
        for l:object in l:data[l:type]['data']
            let l:line = get(l:object, 'line', -1)
            let l:message = l:object['message']

            let l:link = get(l:object, 'reference_url', '')

            if type(l:link) == v:t_list
                " Somehow, reference_url is returned as two-part list.
                " Anchor markers in that list are sometimes duplicated.
                " See https://github.com/projectatomic/dockerfile_lint/issues/134
                let l:link = join(l:link, '')
                let l:link = substitute(l:link, '##', '#', '')
            endif

            let l:detail = l:message

            if get(l:object, 'description', 'None') isnot# 'None'
                let l:detail .= "\n\n" . l:object['description']
            endif

            let l:detail .= "\n\n" . l:link

            call add(l:messages, {
            \   'lnum': l:line,
            \   'text': l:message,
            \   'type': ale_linters#dockerfile#dockerfile_lint#GetType(l:type),
            \   'detail': l:detail,
            \})
        endfor
    endfor

    return l:messages
endfunction

function! ale_linters#dockerfile#dockerfile_lint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'dockerfile_dockerfile_lint_options'))
    \   . ' -p -j -f'
    \   . ' %t'
endfunction

call ale#linter#Define('dockerfile', {
\   'name': 'dockerfile_lint',
\   'executable': {b -> ale#Var(b, 'dockerfile_dockerfile_lint_executable')},
\   'command': function('ale_linters#dockerfile#dockerfile_lint#GetCommand'),
\   'callback': 'ale_linters#dockerfile#dockerfile_lint#Handle',
\})
