" Author: keith <k@keith.so>
" Description: pylint for python files

let g:ale_python_pylint_executable =
\   get(g:, 'ale_python_pylint_executable', 'pylint')

let g:ale_python_pylint_options =
\   get(g:, 'ale_python_pylint_options', '')

let g:ale_python_pylint_use_global = get(g:, 'ale_python_pylint_use_global', 0)

function! ale_linters#python#pylint#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_pylint', ['pylint'])
endfunction

function! ale_linters#python#pylint#GetCommand(buffer) abort
    return ale#Escape(ale_linters#python#pylint#GetExecutable(a:buffer))
    \   . ' ' . ale#Var(a:buffer, 'python_pylint_options')
    \   . ' --output-format text --msg-template="{path}:{line}:{column}: {msg_id} ({symbol}) {msg}" --reports n'
    \   . ' %s'
endfunction

function! ale_linters#python#pylint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " test.py:4:4: W0101 (unreachable) Unreachable code
    let l:pattern = '\v^[^:]+:(\d+):(\d+): ([[:alnum:]]+) \(([^(]*)\) (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        "let l:failed = append(0, l:match)
        let l:code = l:match[3]

        if (l:code is# 'C0303')
        \ && !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
            " Skip warnings for trailing whitespace if the option is off.
            continue
        endif

        if l:code is# 'I0011'
            " Skip 'Locally disabling' message
             continue
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 1,
        \   'text': l:match[5],
        \   'code': l:match[4],
        \   'type': l:code[:0] is# 'E' ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'pylint',
\   'executable_callback': 'ale_linters#python#pylint#GetExecutable',
\   'command_callback': 'ale_linters#python#pylint#GetCommand',
\   'callback': 'ale_linters#python#pylint#Handle',
\   'lint_file': 1,
\})
