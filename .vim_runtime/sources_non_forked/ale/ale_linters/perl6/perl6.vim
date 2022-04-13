" Author:Travis Gibson <https://github.com/Garland-g>
" Description: This file adds support for checking perl6 syntax

let g:ale_perl6_perl6_executable =
\   get(g:, 'ale_perl6_perl6_executable', 'perl6')

let g:ale_perl6_perl6_options =
\   get(g:, 'ale_perl6_perl6_options', '-c -Ilib')

let $PERL6_EXCEPTIONS_HANDLER = 'JSON'

let $RAKUDO_ERROR_COLOR = 0

function! ale_linters#perl6#perl6#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'perl6_perl6_executable')
endfunction

function! ale_linters#perl6#perl6#GetCommand(buffer) abort
    return ale_linters#perl6#perl6#GetExecutable(a:buffer)
    \   . ' ' . ale#Var(a:buffer, 'perl6_perl6_options')
    \   . ' %t'
endfunction

function! ale_linters#perl6#perl6#ExtractError(dict, item, type, buffer) abort
    let l:file = ''
    let l:line = 1
    let l:column = ''
    let l:text = ''
    let l:pre = ''
    let l:counter = 2
    let l:end_line = ''
    let l:linepatternmessage = 'at\s\+line\s\+\(\d\+\)'

    if has_key(a:dict[a:item], 'filename') && !empty(a:dict[a:item]['filename'])
        let l:file = a:dict[a:item]['filename']
    endif

    if has_key(a:dict[a:item], 'line') && !empty(a:dict[a:item]['line'])
        let l:line = a:dict[a:item]['line']
        let l:counter -= 1
    endif

    if has_key(a:dict[a:item], 'column') && !empty(a:dict[a:item]['column'])
        let l:column = a:dict[a:item]['column']
    endif

    if has_key(a:dict[a:item], 'message') && !empty(a:dict[a:item]['message'])
        let l:text = substitute(a:dict[a:item]['message'], '\s*\n\s*', ' ', 'g')
        let l:counter -= 1
    endif

    if has_key(a:dict[a:item], 'line-real') && !empty(a:dict[a:item]['line-real'])
        let l:end_line = l:line
        let l:line = a:dict[a:item]['line-real']
    endif

    for l:match in ale#util#GetMatches(l:text, l:linepatternmessage)
        let l:line = l:match[1]
        let l:counter -= 1
    endfor

" Currently, filenames and line numbers are not always given in the error output
    if l:counter < 2
    \&& ( ale#path#IsBufferPath(a:buffer, l:file) || l:file is# '' )
        return {
        \   'lnum': '' . l:line,
        \   'text': l:text,
        \   'type': a:type,
        \   'col': l:column,
        \   'end_lnum': l:end_line,
        \   'code': a:item,
        \}
    endif

    return ''
endfunction

function! ale_linters#perl6#perl6#Handle(buffer, lines) abort
    let l:output = []

    if empty(a:lines)
        return l:output
    endif

    if a:lines[0] is# 'Syntax OK'
        return l:output
    endif

    try
        let l:json = json_decode(join(a:lines, ''))
    catch /E474\|E491/
        call add(l:output, {
        \   'lnum': '1',
        \   'text': 'Received output in the default Perl6 error format. See :ALEDetail for details',
        \   'detail': join(a:lines, "\n"),
        \   'type': 'W',
        \   })

        return l:output
    endtry

    if type(l:json) is v:t_dict
        for l:key in keys(l:json)
            if has_key(l:json[l:key], 'sorrows')
            \&& has_key(l:json[l:key], 'worries')
                if !empty(l:json[l:key]['sorrows'])
                    for l:dictionary in get(l:json[l:key], 'sorrows')
                        for l:item in keys(l:dictionary)
                            let l:result =
                            \   ale_linters#perl6#perl6#ExtractError(
                            \       l:dictionary,
                            \       l:item,
                            \       'E',
                            \       a:buffer,
                            \   )

                            if l:result isnot# ''
                                call add(l:output, l:result)
                            endif
                        endfor
                    endfor
                endif

                if !empty(l:json[l:key]['worries'])
                    for l:dictionary in get(l:json[l:key], 'worries')
                        for l:item in keys(l:dictionary)
                            let l:result =
                            \   ale_linters#perl6#perl6#ExtractError(
                            \       l:dictionary,
                            \       l:item,
                            \       'W',
                            \       a:buffer,
                            \   )

                            if l:result isnot# ''
                                call add(l:output, l:result)
                            endif
                        endfor
                    endfor
                endif
            else
                let l:result = ale_linters#perl6#perl6#ExtractError(
                \     l:json,
                \     l:key,
                \     'E',
                \     a:buffer,
                \   )

                if l:result isnot# ''
                    call add(l:output, l:result)
                endif
            endif
        endfor
    endif

    return l:output
endfunction

call ale#linter#Define('perl6', {
\   'name': 'perl6',
\   'executable': function('ale_linters#perl6#perl6#GetExecutable'),
\   'output_stream': 'both',
\   'command': function('ale_linters#perl6#perl6#GetCommand'),
\   'callback': 'ale_linters#perl6#perl6#Handle',
\})

