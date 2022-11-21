" Author: Alexander Olofsson <alexander.olofsson@liu.se>

call ale#Set('puppet_puppet_executable', 'puppet')
call ale#Set('puppet_puppet_options', '')

function! ale_linters#puppet#puppet#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " Error: Could not parse for environment production: Syntax error at ':' at /root/puppetcode/modules/nginx/manifests/init.pp:43:12
    " Error: Could not parse for environment production: Syntax error at '='; expected '}' at /root/puppetcode/modules/pancakes/manifests/init.pp:5"
    " Error: Could not parse for environment production: Syntax error at 'parameter1' (file: /tmp/modules/mariadb/manifests/slave.pp, line: 4, column: 5)
    " Error: Illegal attempt to assign to 'a Name'. Not an assignable reference (file: /tmp/modules/waffles/manifests/syrup.pp, line: 5, column: 11)
    " Error: Could not parse for environment production: Syntax error at end of input (file: /tmp/modules/bob/manifests/init.pp)
    let l:pattern = '^Error:\%(.*:\)\? \(.\+\) \((file:\|at\) .\+\.pp\(\(, line: \|:\)\(\d\+\)\(, column: \|:\)\=\(\d*\)\|)$\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[5] + 0,
        \   'col': l:match[7] + 0,
        \   'text': l:match[1],
        \})
    endfor

    return l:output
endfunction

function! ale_linters#puppet#puppet#GetCommand(buffer) abort
    return '%e parser validate --color=false '
    \    . ale#Pad(ale#Var(a:buffer, 'puppet_puppet_options'))
    \    . ' %t'
endfunction

call ale#linter#Define('puppet', {
\   'name': 'puppet',
\   'executable': {b -> ale#Var(b, 'puppet_puppet_executable')},
\   'output_stream': 'stderr',
\   'command': function('ale_linters#puppet#puppet#GetCommand'),
\   'callback': 'ale_linters#puppet#puppet#Handle',
\})
