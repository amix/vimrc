" Author: Dmitri Vereshchagin <dmitri.vereshchagin@gmail.com>
" Description: SyntaxErl linter for Erlang files

call ale#Set('erlang_syntaxerl_executable', 'syntaxerl')

function! ale_linters#erlang#syntaxerl#Handle(buffer, lines) abort
    let l:pattern = '\v\C:(\d+):( warning:)? (.+)'
    let l:loclist = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:loclist, {
        \   'lnum': str2nr(l:match[1]),
        \   'text': l:match[3],
        \   'type': empty(l:match[2]) ? 'E' : 'W',
        \})
    endfor

    return l:loclist
endfunction

function! s:GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'erlang_syntaxerl_executable')
endfunction

function! s:GetCommand(buffer) abort
    let l:Callback = function('s:GetCommandFromHelpOutput')

    return ale#command#Run(a:buffer, '%e -h', l:Callback, {
    \   'executable': s:GetExecutable(a:buffer),
    \})
endfunction

function! s:GetCommandFromHelpOutput(buffer, output, metadata) abort
    let l:has_b_option = match(a:output, '\V\C-b, --base\>') > -1

    return l:has_b_option ? '%e -b %s %t' : '%e %t'
endfunction

call ale#linter#Define('erlang', {
\   'name': 'syntaxerl',
\   'callback': 'ale_linters#erlang#syntaxerl#Handle',
\   'executable': function('s:GetExecutable'),
\   'command': function('s:GetCommand'),
\})
