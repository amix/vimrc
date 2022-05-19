call ale#Set('lua_selene_executable', 'selene')
call ale#Set('lua_selene_options', '')

function! ale_linters#lua#selene#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'lua_selene_options'))
    \   . ' --display-style=json -'
endfunction

function! ale_linters#lua#selene#Handle(buffer, lines) abort
    let l:output = []

    for l:line in a:lines
        " as of version 0.17.0, selene has no way to suppress summary
        " information when outputting json, so stop processing when we hit it
        " (PR for this here: https://github.com/Kampfkarren/selene/pull/356)
        if l:line is# 'Results:'
            break
        endif

        let l:json = json_decode(l:line)
        let l:lint = {
        \   'lnum': l:json.primary_label.span.start_line + 1,
        \   'end_lnum': l:json.primary_label.span.end_line + 1,
        \   'col': l:json.primary_label.span.start_column + 1,
        \   'end_col': l:json.primary_label.span.end_column,
        \   'text': l:json.message,
        \   'code': l:json.code,
        \   'type': l:json.severity is# 'Warning' ? 'W' : 'E',
        \}

        if has_key(l:json, 'notes') && len(l:json.notes) > 0
            let l:lint.detail = l:lint.text . "\n\n" . join(l:json.notes, "\n")
        endif

        call add(l:output, l:lint)
    endfor

    return l:output
endfunction

call ale#linter#Define('lua', {
\   'name': 'selene',
\   'executable': {b -> ale#Var(b, 'lua_selene_executable')},
\   'command': function('ale_linters#lua#selene#GetCommand'),
\   'callback': 'ale_linters#lua#selene#Handle',
\})
