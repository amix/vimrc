scriptencoding utf-8
" Author: David Houston <houstdav000>
" Description: Define a handler function for cspell's output

function! ale#handlers#cspell#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer,
    \    'cspell', [
    \        'node_modules/.bin/cspell',
    \        'node_modules/cspell/bin.js',
    \    ]
    \)
endfunction

function! ale#handlers#cspell#GetLanguageId(buffer) abort
    let l:filetype = getbufvar(a:buffer, '&filetype')

    if l:filetype is# 'tex'
        " Vim's tex corresponds to latex language-id in cspell
        return 'latex'
    elseif l:filetype is# 'plaintex'
        " Vim's plaintex corresponds to tex language-id in cspell
        return 'tex'
    else
        " Fallback to filetype for everything else.
        return l:filetype
    endif
endfunction

function! ale#handlers#cspell#GetCommand(buffer) abort
    let l:executable = ale#handlers#cspell#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'cspell_options')
    let l:language_id = ale#handlers#cspell#GetLanguageId(a:buffer)

    let l:language_id_option = empty(l:language_id) ? '' : '--language-id="' . l:language_id . '"'

    return ale#node#Executable(a:buffer, l:executable)
    \   . ' lint --no-color --no-progress --no-summary'
    \   . ale#Pad(l:language_id_option)
    \   . ale#Pad(l:options)
    \   . ' -- stdin'
endfunction

function! ale#handlers#cspell#Handle(buffer, lines) abort
    " Look for lines like the following:
    "
    " /home/user/repos/ale/README.md:3:128 - Unknown word (Neovim)
    " match1: 3
    " match2: 128
    " match3: Unknown word (Neovim)
    " match4: Neovim
    let l:pattern = '\v^.*:(\d+):(\d+) - ([^\(]+\(([^\)]+)\).*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'end_col': l:match[2] + len(l:match[4]) - 1,
        \   'text': l:match[3],
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

function! ale#handlers#cspell#DefineLinter(filetype) abort
    call ale#Set('cspell_executable', 'cspell')
    call ale#Set('cspell_options', '')
    call ale#Set('cspell_use_global', get(g:, 'ale_use_global_executables', 0))

    call ale#linter#Define(a:filetype, {
    \   'name': 'cspell',
    \   'executable': function('ale#handlers#cspell#GetExecutable'),
    \   'command': function('ale#handlers#cspell#GetCommand'),
    \   'callback': 'ale#handlers#cspell#Handle',
    \})
endfunction
