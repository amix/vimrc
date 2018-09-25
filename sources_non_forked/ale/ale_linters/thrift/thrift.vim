" Author: Jon Parise <jon@indelible.org>

call ale#Set('thrift_thrift_executable', 'thrift')
call ale#Set('thrift_thrift_generators', ['cpp'])
call ale#Set('thrift_thrift_includes', ['.'])
call ale#Set('thrift_thrift_options', '-strict')

function! ale_linters#thrift#thrift#GetCommand(buffer) abort
    let l:generators = ale#Var(a:buffer, 'thrift_thrift_generators')
    let l:includes = ale#Var(a:buffer, 'thrift_thrift_includes')

    " The thrift compiler requires at least one generator. If none are set,
    " fall back to our default value to avoid silently failing. We could also
    " `throw` here, but that seems even less helpful.
    if empty(l:generators)
        let l:generators = ['cpp']
    endif

    let l:output_dir = ale#engine#CreateDirectory(a:buffer)

    return '%e'
    \   . ale#Pad(join(map(copy(l:generators), "'--gen ' . v:val")))
    \   . ale#Pad(join(map(copy(l:includes), "'-I ' . v:val")))
    \   . ale#Pad(ale#Var(a:buffer, 'thrift_thrift_options'))
    \   . ' -out ' . ale#Escape(l:output_dir)
    \   . ' %t'
endfunction

function! ale_linters#thrift#thrift#Handle(buffer, lines) abort
    " Matches lines like the following:
    "
    " [SEVERITY:/path/filename.thrift:31] Message text
    " [ERROR:/path/filename.thrift:31] (last token was ';')
    let l:pattern = '\v^\[(\u+):(.*):(\d+)\] (.*)$'

    let l:index = 0
    let l:output = []

    " Roll our own output-matching loop instead of using ale#util#GetMatches
    " because we need to support error messages that span multiple lines.
    while l:index < len(a:lines)
        let l:line = a:lines[l:index]

        let l:match = matchlist(l:line, l:pattern)

        if empty(l:match)
            let l:index += 1
            continue
        endif

        let l:severity = l:match[1]

        if l:severity is# 'WARNING'
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        " If our text looks like "(last token was ';')", the *next* line
        " should contain a more descriptive error message.
        let l:text = l:match[4]

        if l:text =~# '\(last token was .*\)'
            let l:index += 1
            let l:text = get(a:lines, l:index, 'Unknown error ' . l:text)
        endif

        call add(l:output, {
        \   'lnum': l:match[3] + 0,
        \   'col': 0,
        \   'type': l:type,
        \   'text': l:text,
        \})

        let l:index += 1
    endwhile

    return l:output
endfunction

call ale#linter#Define('thrift', {
\   'name': 'thrift',
\   'executable': 'thrift',
\   'output_stream': 'both',
\   'executable_callback': ale#VarFunc('thrift_thrift_executable'),
\   'command_callback': 'ale_linters#thrift#thrift#GetCommand',
\   'callback': 'ale_linters#thrift#thrift#Handle',
\})
