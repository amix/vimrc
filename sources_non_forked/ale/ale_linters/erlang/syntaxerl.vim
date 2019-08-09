" Author: Dmitri Vereshchagin <dmitri.vereshchagin@gmail.com>
" Description: SyntaxErl linter for Erlang files

call ale#Set('erlang_syntaxerl_executable', 'syntaxerl')

function! ale_linters#erlang#syntaxerl#RunHelpCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'erlang_syntaxerl_executable')

    return ale#command#Run(
    \   a:buffer,
    \   ale#Escape(l:executable) . ' -h',
    \   function('ale_linters#erlang#syntaxerl#GetCommand'),
    \)
endfunction

function! ale_linters#erlang#syntaxerl#GetCommand(buffer, output, meta) abort
    let l:use_b_option = match(a:output, '\C\V-b, --base\>') > -1

    return '%e' . (l:use_b_option ? ' -b %s %t' : ' %t')
endfunction

function! ale_linters#erlang#syntaxerl#Handle(buffer, lines) abort
    let l:pattern = '\v\C:(\d+):( warning:)? (.+)'
    let l:loclist = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:loclist, {
        \   'lnum': l:match[1] + 0,
        \   'text': l:match[3],
        \   'type': empty(l:match[2]) ? 'E' : 'W',
        \})
    endfor

    return l:loclist
endfunction

call ale#linter#Define('erlang', {
\   'name': 'syntaxerl',
\   'executable': {b -> ale#Var(b, 'erlang_syntaxerl_executable')},
\   'command': {b -> ale_linters#erlang#syntaxerl#RunHelpCommand(b)},
\   'callback': 'ale_linters#erlang#syntaxerl#Handle',
\})
