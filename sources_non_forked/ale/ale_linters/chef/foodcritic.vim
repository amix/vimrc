" Author: Edward Larkey <edwlarkey@mac.com>
" Author: Jose Junior <jose.junior@gmail.com>
" Author: w0rp <devw0rp@gmail.com>
" Description: This file adds the foodcritic linter for Chef files.

call ale#Set('chef_foodcritic_executable', 'foodcritic')
call ale#Set('chef_foodcritic_options', '')

function! ale_linters#chef#foodcritic#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'chef_foodcritic_options')

    return '%e' . ale#Pad(escape(l:options, '~')) . ' %s'
endfunction

function! ale_linters#chef#foodcritic#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " FC002: Avoid string interpolation where not required: httpd.rb:13
    let l:pattern = '\v([^:]+): (.+): ([a-zA-Z]?:?[^:]+):(\d+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'code': l:match[1],
        \   'text': l:match[2],
        \   'filename': l:match[3],
        \   'lnum': l:match[4] + 0,
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('chef', {
\   'name': 'foodcritic',
\   'executable_callback': ale#VarFunc('chef_foodcritic_executable'),
\   'command_callback': 'ale_linters#chef#foodcritic#GetCommand',
\   'callback': 'ale_linters#chef#foodcritic#Handle',
\   'lint_file': 1,
\})
