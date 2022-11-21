" Author: offa
" Description: oelint-adv for BitBake files

call ale#Set('bitbake_oelint_adv_executable', 'oelint-adv')
call ale#Set('bitbake_oelint_adv_options', '')
call ale#Set('bitbake_oelint_adv_config', '.oelint.cfg')

function! ale_linters#bitbake#oelint_adv#Command(buffer) abort
    let l:config_file = ale#path#FindNearestFile(a:buffer,
    \    ale#Var(a:buffer, 'bitbake_oelint_adv_config'))

    return ((!empty(l:config_file))
    \    ? 'OELINT_CONFIG=' . ale#Escape(l:config_file) . ' '
    \    : '')
    \    . '%e --quiet '
    \    . ale#Pad(ale#Var(a:buffer, 'bitbake_oelint_adv_options')) .  '%s'
endfunction

function! ale_linters#bitbake#oelint_adv#Handle(buffer, lines) abort
    let l:pattern = '\v^(.+):(.+):(.+):(.+):(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \    'lnum': str2nr(l:match[2]),
        \    'type': l:match[3] is# 'error'
        \          ? 'E' : (l:match[3] is# 'warning' ? 'W' : 'I'),
        \    'text': StripAnsiCodes(l:match[5]),
        \    'code': l:match[4]
        \    })
    endfor

    return l:output
endfunction

function! StripAnsiCodes(line) abort
    return substitute(a:line, '\e\[[0-9;]\+[mK]', '', 'g')
endfunction

call ale#linter#Define('bitbake', {
\    'name': 'oelint_adv',
\    'output_stream': 'both',
\    'executable': {b -> ale#Var(b, 'bitbake_oelint_adv_executable')},
\    'cwd': '%s:h',
\    'command': function('ale_linters#bitbake#oelint_adv#Command'),
\    'callback': 'ale_linters#bitbake#oelint_adv#Handle',
\    })
