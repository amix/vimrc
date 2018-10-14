" Author: Vincent Lequertier <https://github.com/SkySymbol>, Chris Weyl <cweyl@alumni.drew.edu>
" Description: This file adds support for checking perl with perl critic

call ale#Set('perl_perlcritic_executable', 'perlcritic')
call ale#Set('perl_perlcritic_profile', '.perlcriticrc')
call ale#Set('perl_perlcritic_options', '')
call ale#Set('perl_perlcritic_showrules', 0)

function! ale_linters#perl#perlcritic#GetProfile(buffer) abort
    " first see if we've been overridden
    let l:profile = ale#Var(a:buffer, 'perl_perlcritic_profile')

    if l:profile is? ''
        return ''
    endif

    " otherwise, iterate upwards to find it
    return ale#path#FindNearestFile(a:buffer, l:profile)
endfunction

function! ale_linters#perl#perlcritic#GetCommand(buffer) abort
    let l:critic_verbosity = '%l:%c %m\n'

    if ale#Var(a:buffer, 'perl_perlcritic_showrules')
        let l:critic_verbosity = '%l:%c %m [%p]\n'
    endif

    let l:profile = ale_linters#perl#perlcritic#GetProfile(a:buffer)
    let l:options = ale#Var(a:buffer, 'perl_perlcritic_options')

    return '%e'
    \   . ' --verbose ' . ale#Escape(l:critic_verbosity)
    \   . ' --nocolor'
    \   . (!empty(l:profile) ? ' --profile ' . ale#Escape(l:profile) : '')
    \   . ale#Pad(l:options)
endfunction


function! ale_linters#perl#perlcritic#Handle(buffer, lines) abort
    let l:pattern = '\(\d\+\):\(\d\+\) \(.\+\)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1],
        \   'col': l:match[2],
        \   'text': l:match[3],
        \   'type': 'W'
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('perl', {
\   'name': 'perlcritic',
\   'output_stream': 'stdout',
\   'executable_callback': ale#VarFunc('perl_perlcritic_executable'),
\   'command_callback': 'ale_linters#perl#perlcritic#GetCommand',
\   'callback': 'ale_linters#perl#perlcritic#Handle',
\})
