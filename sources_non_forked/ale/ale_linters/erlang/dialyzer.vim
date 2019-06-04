" Author: Autoine Gagne - https://github.com/AntoineGagne
" Description: Define a checker that runs dialyzer on Erlang files.

let g:ale_erlang_dialyzer_executable =
\   get(g:, 'ale_erlang_dialyzer_executable', 'dialyzer')
let g:ale_erlang_dialyzer_plt_file =
\   get(g:, 'ale_erlang_dialyzer_plt_file', '')
let g:ale_erlang_dialyzer_rebar3_profile =
\   get(g:, 'ale_erlang_dialyzer_rebar3_profile', 'default')

function! ale_linters#erlang#dialyzer#GetRebar3Profile(buffer) abort
    return ale#Var(a:buffer, 'erlang_dialyzer_rebar3_profile')
endfunction

function! ale_linters#erlang#dialyzer#FindPlt(buffer) abort
    let l:plt_file = ''
    let l:rebar3_profile = ale_linters#erlang#dialyzer#GetRebar3Profile(a:buffer)
    let l:plt_file_directory = ale#path#FindNearestDirectory(a:buffer, '_build' . l:rebar3_profile)

    if !empty(l:plt_file_directory)
        let l:plt_file = split(globpath(l:plt_file_directory, '/*_plt'), '\n')
    endif

    if !empty(l:plt_file)
        return l:plt_file[0]
    endif

    if !empty($REBAR_PLT_DIR)
        return expand('$REBAR_PLT_DIR/dialyzer/plt')
    endif

    return expand('$HOME/.dialyzer_plt')
endfunction

function! ale_linters#erlang#dialyzer#GetPlt(buffer) abort
    let l:plt_file = ale#Var(a:buffer, 'erlang_dialyzer_plt_file')

    if !empty(l:plt_file)
        return l:plt_file
    endif

    return ale_linters#erlang#dialyzer#FindPlt(a:buffer)
endfunction

function! ale_linters#erlang#dialyzer#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'erlang_dialyzer_executable')
endfunction

function! ale_linters#erlang#dialyzer#GetCommand(buffer) abort
    let l:command = ale#Escape(ale_linters#erlang#dialyzer#GetExecutable(a:buffer))
    \   . ' -n'
    \   . ' --plt ' . ale#Escape(ale_linters#erlang#dialyzer#GetPlt(a:buffer))
    \   . ' -Wunmatched_returns'
    \   . ' -Werror_handling'
    \   . ' -Wrace_conditions'
    \   . ' -Wunderspecs'
    \   . ' %s'

    return l:command
endfunction

function! ale_linters#erlang#dialyzer#Handle(buffer, lines) abort
    " Match patterns like the following:
    "
    " erl_tidy_prv_fmt.erl:3: Callback info about the provider behaviour is not available
    let l:pattern = '^\S\+:\(\d\+\): \(.\+\)$'
    let l:output = []

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) != 0
            let l:code = l:match[2]

            call add(l:output, {
            \   'lnum': str2nr(l:match[1]),
            \   'lcol': 0,
            \   'text': l:code,
            \   'type': 'W'
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('erlang', {
\   'name': 'dialyzer',
\   'executable': function('ale_linters#erlang#dialyzer#GetExecutable'),
\   'command': function('ale_linters#erlang#dialyzer#GetCommand'),
\   'callback': function('ale_linters#erlang#dialyzer#Handle'),
\   'lint_file': 1
\})
