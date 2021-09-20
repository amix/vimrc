" Author: fiatjaf <fiatjaf@alhur.es>
" Description: v build for V files

call ale#Set('v_v_executable', 'v')
call ale#Set('v_v_options', '')

function! ale_linters#v#v#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'v_v_options')

    " Run v in local directory with relative path
    let l:command = ale#Var(a:buffer, 'v_v_executable')
    \   . ale#Pad(l:options)
    \   . ' .' . ' -o /tmp/vim-ale-v'

    return l:command
endfunction

function! ale_linters#v#v#Handler(buffer, lines) abort
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:output = []

    " Matches patterns like the following:
    "
    " ./const.v:4:3: warning: const names cannot contain uppercase letters, use snake_case instead
    "     2 |
    "     3 | const (
    "     4 |   BUTTON_TEXT = 'OK'
    "       |   ~~~~~~~~~~~
    "     5 | )
    " ./main.v:4:8: warning: module 'os' is imported but never used
    "     2 |
    "     3 | import ui
    "     4 | import os
    "       |        ~~
    "     5 |
    "     6 | const (
    " ./main.v:20:10: error: undefined ident: `win_widt`
    "    18 |     mut app := &App{}
    "    19 |     app.window = ui.window({
    "    20 |         width: win_widt
    "       |                ~~~~~~~~
    "    21 |         height: win_height
    "    22 |         title: 'Counter'
    let l:current = {}

    for l:line in a:lines
        " matches basic error description
        let l:match = matchlist(l:line,
        \ '\([^:]\+\):\([^:]\+\):\([^:]\+\): \([^:]\+\): \(.*\)')

        if !empty(l:match)
            let l:current = {
            \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
            \   'lnum': l:match[2] + 0,
            \   'col': l:match[3] + 0,
            \   'text': l:match[5],
            \   'type': l:match[4] is# 'error' ? 'E' : 'W',
            \}
            call add(l:output, l:current)
            continue
        endif

        " try to get information about the ending column
        let l:tildematch = matchstr(l:line, '\~\+')

        if !empty(l:tildematch)
            let l:current['end_col'] = l:current['col'] + len(l:tildematch)
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('v', {
\   'name': 'v',
\   'aliases': [],
\   'executable': {b -> ale#Var(b, 'v_v_executable')},
\   'command': function('ale_linters#v#v#GetCommand'),
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#v#v#Handler',
\   'lint_file': 1,
\})
